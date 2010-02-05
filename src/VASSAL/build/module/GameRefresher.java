/*
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.build.module;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import javax.swing.AbstractAction;
import javax.swing.Action;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.GameModule;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.command.RemovePiece;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Deck;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.PlaceMarker;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

/**
 * GameRefresher
 * Replace all counters in the same game with the current
 * version of the counters defined in the module 
 * 
 * Note: Counters that are Hidden or Obscured to us
 * cannot be updated.  
 *
 */
public final class GameRefresher implements GameComponent {
  
  private Action refreshAction; 
  protected HashMap<String, SlotElement> idMap = new HashMap<String, SlotElement>();
  protected HashMap<String, SlotElement> typeMap = new HashMap<String, SlotElement>(); 

  public GameRefresher() {
  }
  
  public void addTo(AbstractConfigurable parent) {
    refreshAction = new AbstractAction(
        Resources.getString("GameRefresher.refresh_counters")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        new GameRefresher().execute();
      }
    };
    refreshAction.setEnabled(false);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }
  
  public Action getRefreshAction() {
    return refreshAction;
  }
  
  public void execute() {
    
    /*
     * Stage 1
     *  Build a cross-reference of all PieceSlots in the module
     */
    if (! buildIdMap()) {
      ErrorDialog.show("GameRefresher.no_gpids"); //$NON-NLS-1$
      return;
    }
    
    /*
     * Stage 2
     * Process each piece on the map and collect the commands to update
     * the pieces
     */
    final Command command = new NullCommand();
    final ArrayList<GamePiece> pieces = new ArrayList<GamePiece>();
    
    for (GamePiece piece : GameModule.getGameModule().getGameState().getAllPieces()) {
      if (piece instanceof Deck) {
        for (Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext(); ) {
          pieces.add(0, i.next());
        }
      }
      else if (piece instanceof Stack) {
        for (Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext(); ) {
          final GamePiece p = i.next();
          if (! Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME)) &&
              ! Boolean.TRUE.equals(p.getProperty(Properties.OBSCURED_TO_ME))) {
            pieces.add(0, p);
          }          
        }
      }
      else if (piece.getParent() == null) {
        if (! Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME)) &&
            ! Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME))) {
          pieces.add(0, piece);
        }
      }
    }
    
    for (GamePiece piece : pieces) {
      processGamePiece(piece, command);
    }
    
    /*
     * Stage 3
     * Send the update to other clients
     */
    GameModule.getGameModule().sendAndLog(command);
  }
  
  private boolean buildIdMap() {
    for (PieceSlot slot : GameModule.getGameModule().getAllDescendantComponentsOf(PieceSlot.class)) {
      final String id = slot.getGpId(); 
      if (id.length() == 0) {
        return false;
      }
      final SlotElement se = new SlotElement(slot);
      idMap.put(id, se);
      typeMap.put(slot.getPiece().getType(), se);
      
      GamePiece piece = slot.getPiece();
      if (!findPlaceMarkerTraits(piece)) {
        return false;      
      }
    }
    return true;
  }
  
  /**
   * Process a single game piece and replace it with an updated
   * version if possible. Append the commands required to replace
   * the piece to the supplied Command.
   * If no new version is available, replace it with itself to ensure 
   * the Stacking order does not change.
   * 
   * @param piece GamePiece to replace
   * @param command Command List.
   */
  private void processGamePiece(GamePiece piece, Command command) {
    
    final Map map = piece.getMap();
    final Point pos = piece.getPosition();
    final SlotElement se = findSlot(piece); 
    if (se != null) {

      final GamePiece newPiece = se.createPiece(piece);   
      final Stack oldStack = piece.getParent();
      final int oldPos = oldStack.indexOf(piece);
  
      // Place the new Piece. 
      final Command place = map.placeOrMerge(newPiece, pos);
      command.append(place);

      // Remove the old Piece
      final Command remove = new RemovePiece(Decorator.getOutermost(piece));
      remove.execute();
      command.append(remove);
      
      // If still in the same stack, move to correct position
      final Stack newStack = newPiece.getParent();    
      if (newStack != null && oldStack != null && newStack == oldStack) {
        final int newPos = newStack.indexOf(newPiece);        
        if (newPos >= 0 && oldPos >= 0 && newPos != oldPos) {
          final String oldState = newStack.getState();
          newStack.insert(newPiece, oldPos);
          command.append(new ChangePiece(newStack.getId(), oldState, newStack.getState()));     
        }
      }
    }
  }
  
  /**
   * Search all available SlotElements for one that can be used to
   * recreate the specified GamePiece.
   *  1. Find the PieceSlot that created this piece directly by looking up the GpId
   *  2. Fallback, find a PieceSlot with an identically Typed piece
   * 
   * @param piece Source GamePiece
   * @return Found SlotElement
   */
  protected SlotElement findSlot(GamePiece piece) {
    SlotElement se = null;
    final String gpid = (String) piece.getProperty(Properties.PIECE_ID);
    if (gpid != null && gpid.length() > 0) {
      se = idMap.get(gpid);
    }
    if (se == null) {
      se = typeMap.get(piece.getType());
    }
    return se;
  }
  
  /**
   * Search a GamePiece for PlaceMarker traits and record them
   * in our source list of potential new pieces.
   * 
   * @param piece GamePiece to search within
   * @return false if a PlaceMarker is found with no gpid
   */
  protected boolean findPlaceMarkerTraits(GamePiece piece) {
    if (piece == null || piece instanceof BasicPiece) {
      return true;
    }
    if (piece instanceof PlaceMarker) {
      final PlaceMarker pm = (PlaceMarker) piece;
      final String id = pm.getGpId();
      if (id.length() == 0) {
        return false;
      }
      idMap.put(id, new SlotElement(pm));          
    }
    return findPlaceMarkerTraits(((Decorator) piece).getInner());
  }
  
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * Enable Refresh menu item when game is running only.
   */
  public void setup(boolean gameStarting) {
    refreshAction.setEnabled(gameStarting);    
  }
  
  /*********************************************************************
   * Wrapper class for gpid-able components - They will all be either
   * PieceSlot components or PlaceMarker Decorator's
   * 
   * @author Brent Easton
   *
   */
  class SlotElement {
    
    private PieceSlot slot;
    private PlaceMarker marker;
    private String id;
    
    public SlotElement() {
      slot = null;
      marker = null;
    }

    public SlotElement(PieceSlot ps) {
      this();
      slot = ps;
      id = ps.getGpId();
    }
    
    public SlotElement(PlaceMarker pm) {
      this();
      marker = pm;
      id = pm.getGpId();
    }
    
    public String getGpId() {
      return id;
    }
    
    public PieceSlot getSlot() {
      return slot;
    }
    
    public PlaceMarker getMarker() {
      return marker;
    }
    
    /**
     * Create a new GamePiece based on this Slot Element. Use oldPiece
     * to copy state information over to the new piece.
     * 
     * @param oldPiece Old Piece for state information
     * @return New Piece
     */
    public GamePiece createPiece(GamePiece oldPiece) {
      GamePiece newPiece;
      if (slot != null) {
        newPiece = slot.getPiece();
      }
      else {
        newPiece = marker.createMarker();
      }
      newPiece = ((AddPiece) GameModule.getGameModule()
          .decode(GameModule.getGameModule().encode(new AddPiece(newPiece)))).getTarget();
      newPiece = PieceCloner.getInstance().clonePiece(newPiece);
      copyState(oldPiece, newPiece);
      newPiece.setProperty(Properties.PIECE_ID, getGpId());
      return newPiece;
    }

    /**
     * Copy as much state information as possible from the old
     * piece to the new piece
     * 
     * @param oldPiece Piece to copy state from
     * @param newPiece Piece to copy state to
     */
    protected void copyState(GamePiece oldPiece, GamePiece newPiece) {
      GamePiece p = newPiece;
      while (p != null) {
        if (p instanceof BasicPiece) {
          ((BasicPiece) p).setState(((BasicPiece) Decorator.getInnermost(oldPiece)).getState());          
          p = null;
        }
        else {
          final Decorator decorator = (Decorator) p;
          final String type = decorator.myGetType();
          final String newState = findStateFromType(oldPiece, type, p.getClass());
          if (newState != null && newState.length() > 0) {
            decorator.mySetState(newState);
          }
          p = decorator.getInner();
        }
      }      
    }
    
    /**
     * Locate a Decorator in the old piece that has the exact same
     * type as the new Decorator and return it's state
     * 
     * @param oldPiece Old piece to search
     * @param typeToFind Type to match
     * @param classToFind Class to match
     * @return
     */
    protected String findStateFromType(GamePiece oldPiece, String typeToFind, Class<? extends GamePiece> classToFind) {

      GamePiece p = oldPiece;
      while (p != null && !(p instanceof BasicPiece)) {
        final Decorator d = (Decorator) Decorator.getDecorator(p, classToFind);
        if (d != null) {
          if (d.getClass().equals(classToFind)) {
            if (d.myGetType().equals(typeToFind)) {
              return d.myGetState();
            }
          }
          p = d.getInner();
        }
        else
          p = null;
      }
      return null;
    }
  }

}