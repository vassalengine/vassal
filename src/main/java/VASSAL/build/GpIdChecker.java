/*
 * Copyright (c) 2011-2016 by Brent Easton
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
  protected boolean useName = false;
  protected boolean extensionsLoaded = false;
  final HashMap<String, SlotElement> goodSlots = new HashMap<>();
  final ArrayList<SlotElement> errorSlots = new ArrayList<>();

  public GpIdChecker() {
    this(null);
  }

  public GpIdChecker(GpIdSupport gpIdSupport) {
    this.gpIdSupport = gpIdSupport;
    maxId = -1;
  }

  // This constructor is used by the GameRefresher to refresh a game with extensions possibly loaded
  public GpIdChecker(boolean useName) {
    this();
    this.useName = useName;
    this.extensionsLoaded = true;
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
     *  the Extension Id. Remove the Extension Id and just process the numeric part.
     *  
     *  NOTE: If GpIdChecker is being used by the GameRefesher, then there may be 
     *  extensions loaded, so retain the extension prefix to ensure a correct
     *  unique slot id check.
     */
    if (! extensionsLoaded) {
      if (id.contains(":")) {
        id = id.split(":")[1];
      }
    }

    if (id == null || id.length() == 0) {   // gpid not generated yet?
      errorSlots.add(element);
    }
    else {
      if (goodSlots.get(id) != null) {      // duplicate gpid?
        errorSlots.add(element);
      }
      try {
        if (extensionsLoaded) {
          goodSlots.put(id, element);
          System.out.println("Add Id "+id);
        }
        else {
          final int iid = Integer.parseInt(id);
          goodSlots.put(id, element);         // gpid is good.
          if (iid >= maxId) {
            maxId = iid+1;
          }
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
   * created from that Slot.
   *
   *
   *
   * @param oldPiece
   * @return
   */
  public GamePiece createUpdatedPiece(GamePiece oldPiece) {
    // Find a slot with a matching gpid
    final String gpid = (String) oldPiece.getProperty(Properties.PIECE_ID);
    if (gpid != null && gpid.length() > 0) {
      final SlotElement element = goodSlots.get(gpid);
      if (element != null) {
        return element.createPiece(oldPiece);
      }
    }

    // Failed to find a slot by gpid, try by matching piece name if option selected
    if (useName) {
      final String oldPieceName = Decorator.getInnermost(oldPiece).getName();
      for (SlotElement el : goodSlots.values()) {
        final GamePiece newPiece = el.getPiece();
        final String newPieceName = Decorator.getInnermost(newPiece).getName();
        if (oldPieceName.equals(newPieceName)) {
          return el.createPiece(oldPiece);
        }
      }
    }

    return oldPiece;
  }


  public boolean findUpdatedPiece(GamePiece oldPiece) {
    // Find a slot with a matching gpid
    final String gpid = (String) oldPiece.getProperty(Properties.PIECE_ID);
    if (gpid != null && gpid.length() > 0) {
      final SlotElement element = goodSlots.get(gpid);
      if (element != null) {
        return true;
      }
    }

    // Failed to find a slot by gpid, try by matching piece name if option selected
    if (useName) {
      final String oldPieceName = Decorator.getInnermost(oldPiece).getName();
      for (SlotElement el : goodSlots.values()) {
        final GamePiece newPiece = el.getPiece();
        final String newPieceName = Decorator.getInnermost(newPiece).getName();
        if (oldPieceName.equals(newPieceName)) {
          return true;
        }
      }
    }

    return false;
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

    public GamePiece getPiece() {
      if (slot == null) {
        return marker;
      }
      else {
        return slot.getPiece();
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
      GamePiece newPiece = (slot != null) ? slot.getPiece() : marker.createMarker();
      // The following two steps create a complete new GamePiece with all
      // prototypes expanded
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
          p.setState(Decorator.getInnermost(oldPiece).getState());
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