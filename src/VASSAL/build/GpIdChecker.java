/*
 * Copyright (c) 2011 by Brent Easton
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

package VASSAL.build;

import java.util.ArrayList;
import java.util.HashMap;

import VASSAL.build.widget.PieceSlot;
import VASSAL.command.AddPiece;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.PlaceMarker;
import VASSAL.counters.Properties;

/**
 * Build a cross-reference of all GpId-able elements in a module or ModuleExtension,
 * Check for missing, duplicate or illegal GamePieceId's
 * Update if necessary
 *
 */
public class GpIdChecker {

  protected GpIdSupport gpIdSupport;
  protected int maxId;
  final HashMap<String, SlotElement> goodSlots = new HashMap<String, SlotElement>();
  final ArrayList<SlotElement> errorSlots = new ArrayList<SlotElement>();

  public GpIdChecker() {
    this(null);
  }

  public GpIdChecker(GpIdSupport gpIdSupport) {
    this.gpIdSupport = gpIdSupport;
    maxId = -1;
  }

  /**
   * Add a PieceSlot to our cross-reference and any PlaceMarker
   * traits it contains.
   *
   * @param pieceSlot
   */
  public void add(PieceSlot pieceSlot) {
    testGpId(pieceSlot.getGpId(), new SlotElement(pieceSlot));

    // PlaceMarker traits within the PieceSlot definition also contain GpId's.
    GamePiece gp = pieceSlot.getPiece();
    checkTrait(gp, pieceSlot);
  }

  /**
   * Check for PlaceMarker traits in a GamePiece and add them to
   * the cross-reference
   *
   * @param gp
   * @param slot
   */
  protected void checkTrait(GamePiece gp, PieceSlot slot) {
    if (gp == null || gp instanceof BasicPiece) {
      return;
    }

    if (gp instanceof PlaceMarker) {
       final PlaceMarker pm = (PlaceMarker) gp;
       testGpId (pm.getGpId(), new SlotElement(pm, slot));
    }

    checkTrait(((Decorator) gp).getInner(), slot);

  }

  /**
   * Validate a GamePieceId.
   *  - non-null
   *  - Integer
   *  - Not a duplicate of any other GpId
   *  Keep a list of the good Slots and the slots with errors.
   *  Also track the maximum GpId
   *
   * @param id
   * @param element
   */
  protected void testGpId(String id, SlotElement element) {
    /*
     *  If this has been called from a ModuleExtension, the GpId is prefixed with
     *  the Extension Id. Remove the Extension Id and just process the numerid part.
     */
    if (id.contains(":")) {
      id = id.split(":")[1];
    }

    if (id == null || id.length() == 0) {   // gpid not generated yet?
      errorSlots.add(element);
    }
    else {
      if (goodSlots.get(id) != null) {      // duplicate gpid?
        errorSlots.add(element);
      }
      try {
        final int iid = Integer.parseInt(id);
        goodSlots.put(id, element);         // gpid is good.
        if (iid >= maxId) {
          maxId = iid+1;
        }
      }
      catch (Exception e) {
        errorSlots.add(element);            // non-numeric gpid?
      }
    }
  }

  /**
   * Where any errors found?
   * @return
   */
  public boolean hasErrors() {
    return errorSlots.size() > 0;
  }

  /**
   * Repair any errors
   *  - Update the next GpId in the module if necessary
   *  - Generate new GpId's for slots with errors.
   */
  public void fixErrors() {
    if (maxId >= gpIdSupport.getNextGpId()) {
      gpIdSupport.setNextGpId(maxId+1);
    }
    for (SlotElement slotElement : errorSlots) {
      slotElement.updateGpId();
    }
  }

  /**
   * Locate the SlotElement that matches oldPiece and return a new GamePiece
   * create from that Slot.
   *
   *
   *
   * @param oldPiece
   * @return
   */
  public GamePiece createUpdatedPiece (GamePiece oldPiece) {
    final String gpid = (String) oldPiece.getProperty(Properties.PIECE_ID);
    if (gpid == null) {
      return oldPiece;
    }
    final SlotElement element = goodSlots.get(gpid);
    if (element == null) {
      // No matching PieceSlot found.
      return oldPiece;
    }
    return element.createPiece(oldPiece);
  }



  /**
   * Wrapper class for components that contain a GpId - They will all be either
   * PieceSlot components or PlaceMarker Decorator's.
   * Ideally we would add an interface to these components, but this
   * will break any custom code based on PlaceMarker
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

    public SlotElement(PlaceMarker pm, PieceSlot ps) {
      this();
      marker = pm;
      slot = ps;
      id = pm.getGpId();
    }

    public String getGpId() {
      return id;
    }

    public void updateGpId() {
      if (marker == null) {
        slot.updateGpId();
      }
      else {
        marker.updateGpId();
      }
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
      newPiece = (slot != null) ? slot.getPiece() : marker.createMarker();
      // The following two steps create a complete new GamePiece with all
      // prototypes expanded
     // newPiece = ((AddPiece) GameModule.getGameModule()
     //     .decode(GameModule.getGameModule().encode(new AddPiece(newPiece)))).getTarget();
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