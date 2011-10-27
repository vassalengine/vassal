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
import java.util.Iterator;

import javax.swing.AbstractAction;
import javax.swing.Action;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.GameModule;
import VASSAL.build.GpIdChecker;
import VASSAL.build.GpIdSupport;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;
import VASSAL.counters.Deck;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
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
  protected GpIdSupport gpIdSupport;
  protected GpIdChecker gpIdChecker;

  public GameRefresher(GpIdSupport gpIdSupport) {
    this.gpIdSupport = gpIdSupport;
  }

  public void addTo(AbstractConfigurable parent) {
    refreshAction = new AbstractAction(
        Resources.getString("GameRefresher.refresh_counters")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        new GameRefresher(gpIdSupport).execute();
      }
    };
    refreshAction.setEnabled(false);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public Action getRefreshAction() {
    return refreshAction;
  }

  public void execute() {

    final GameModule theModule = GameModule.getGameModule();
    /*
     * 1. Use the GpIdChecker to build a cross-reference of all available PieceSlots and
     * PlaceMarker's in the module.
     */
    gpIdChecker = new GpIdChecker();
    for (PieceSlot slot : theModule.getAllDescendantComponentsOf(PieceSlot.class)) {
      gpIdChecker.add(slot);
    }
    if (gpIdChecker.hasErrors()) {
      // Any errors should have been resolved by the GpId check at startup, so this error indicates
      // a bug in GpIdChecker.fixErrors().
      ErrorDialog.show("GameRefresher.no_gpids"); //$NON-NLS-1$
      return;
    }

    /*
     * 2. Make a list of all pieces in the game that we have access to
     */
    final Command command = new Chatter.DisplayText(theModule.getChatter(),
        Resources.getString("GameRefresher.counters_refreshed",
            GlobalOptions.getInstance().getPlayerId()));
    command.execute();

    final ArrayList<GamePiece> pieces = new ArrayList<GamePiece>();

    for (GamePiece piece : theModule.getGameState().getAllPieces()) {
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

    /*
     * 3. Generate the commands to update the pieces
     */
    for (GamePiece piece : pieces) {
      processGamePiece(piece, command);
    }

    /*
     * 4. Send the update to other clients and release resources
     */
    theModule.sendAndLog(command);
    gpIdChecker = null;
  }

  private void processGamePiece(GamePiece piece, Command command) {

    final Map map = piece.getMap();
    final Point pos = piece.getPosition();
    GamePiece newPiece = gpIdChecker.createUpdatedPiece(piece);

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

  public Command getRestoreCommand() {
    return null;
  }

  /**
   * Enable Refresh menu item when game is running only.
   */
  public void setup(boolean gameStarting) {
    refreshAction.setEnabled(gameStarting);
  }

}