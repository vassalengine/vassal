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

import VASSAL.build.module.Chatter;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.widget.PieceSlot;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Labeler;
import VASSAL.counters.Marker;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.PlaceMarker;
import VASSAL.counters.Properties;
import VASSAL.i18n.Resources;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Build a cross-reference of all GpId-able elements in a module or ModuleExtension,
 * Check for missing, duplicate or illegal GamePieceId's
 * Update if necessary
 *
 */
public class GpIdChecker {

  protected GpIdSupport gpIdSupport;
  protected int maxId;
  protected boolean extensionsLoaded = false;
  final Map<String, SlotElement> goodSlots = new HashMap<>();
  final List<SlotElement> errorSlots = new ArrayList<>();
  private Chatter chatter;
  private final Set<String> refresherOptions = new HashSet<>();

  public GpIdChecker() {
    this((GpIdSupport) null);
  }

  public GpIdChecker(GpIdSupport gpIdSupport) {
    this.gpIdSupport = gpIdSupport;
    maxId = -1;
  }

  // This constructor is used by the GameRefresher to refresh a game with extensions possibly loaded
  public GpIdChecker(Set<String> options) {
    this();
//    this.useName = useName;
//    this.useLabelerName = useLabelerName;
    this.extensionsLoaded = true;
    if (!options.isEmpty()) {
      this.refresherOptions.addAll(options);
    }
  }

  public boolean useLabelerName() {
    return refresherOptions.contains("UseLabelerName"); //$NON-NLS-1$
  }
  public boolean useLayerName() {
    return refresherOptions.contains("UseLayerName"); //$NON-NLS-1$
  }

  public boolean useName() {
    return refresherOptions.contains("UseName"); //$NON-NLS-1$
  }

  /**
   * Add a PieceSlot to our cross-reference and any PlaceMarker
   * traits it contains.
   *
   * @param pieceSlot PieceSlot to add to cross-reference
   */
  public void add(PieceSlot pieceSlot) {
    testGpId(pieceSlot.getGpId(), new SlotElement(pieceSlot));

    // PlaceMarker traits within the PieceSlot definition also contain GpId's.
    checkTrait(pieceSlot.getPiece());
  }

  /**
   * Add any PieceSlots contained in traits in a Prototype Definition
   * @param prototype Prototype Definition to check
   */
  public void add(PrototypeDefinition prototype) {
    final GamePiece gp = prototype.getPiece();
    checkTrait(gp, prototype, gp);
  }

  /**
   * Check for PlaceMarker traits in a GamePiece and add them to
   * the cross-reference
   *
   * @param gp GamePiece to check
   */
  protected void checkTrait(GamePiece gp) {
    if (gp == null || gp instanceof BasicPiece) {
      return;
    }

    if (gp instanceof PlaceMarker) {
      final PlaceMarker pm = (PlaceMarker) gp;
      testGpId(pm.getGpId(), new SlotElement(pm));
    }

    checkTrait(((Decorator) gp).getInner());

  }

  protected void checkTrait(final GamePiece gp, PrototypeDefinition prototype, GamePiece definition) {
    if (gp == null || gp instanceof BasicPiece) {
      return;
    }

    if (gp instanceof PlaceMarker) {
      final PlaceMarker pm = (PlaceMarker) gp;
      testGpId(pm.getGpId(), new SlotElement(pm, prototype, definition));
    }

    checkTrait(((Decorator) gp).getInner(), prototype, definition);

  }

  /**
   * Validate a GamePieceId.
   *  - non-null
   *  - Integer
   *  - Not a duplicate of any other GpId
   *  Keep a list of the good Slots and the slots with errors.
   *  Also track the maximum GpId
   *
   * @param id GpId to test
   * @param element Containing SlotElement
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
      if (id.contains(":")) {             //NON-NLS
        id = id.split(":")[1];      //NON-NLS
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
        }
        else {
          final int iid = Integer.parseInt(id);
          goodSlots.put(id, element);         // gpid is good.
          if (iid > maxId) {
            maxId = iid;
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
   * @return Error count
   */
  public boolean hasErrors() {
    return !errorSlots.isEmpty();
  }

  private void chat(String text) {
    if (chatter == null) {
      chatter = GameModule.getGameModule().getChatter();
    }
    final Chatter.DisplayText mess = new Chatter.DisplayText(chatter, "- " + text); //NON-NLS
    mess.execute();
  }

  /**
   * Repair any errors
   *  - Update the next GpId in the module if necessary
   *  - Generate new GpId's for slots with errors.
   */
  public void fixErrors() {
    if (maxId >= gpIdSupport.getNextGpId()) {
      chat(Resources.getString("GpIdChecker.next_gpid_updated", gpIdSupport.getNextGpId(), (maxId + 1)));
      gpIdSupport.setNextGpId(maxId + 1);
    }
    for (final SlotElement slotElement : errorSlots) {
      final String before = slotElement.getGpId();
      slotElement.updateGpId();
      chat(Resources.getString("GpIdChecker.piece_gpid_updated", slotElement.toString(), before, slotElement.getGpId()));
    }
  }

  /**
   * Locate the SlotElement that matches oldPiece and return a new GamePiece
   * created from that Slot.
   * Match by ID, if it does not work, match by name if option is ON
   *
   * @param oldPiece Old GamePiece
   * @return Newly created GamePiece
   */
  public GamePiece createUpdatedPiece(GamePiece oldPiece) {
    final String gpid = (String) oldPiece.getProperty(Properties.PIECE_ID);
    GamePiece newPiece;
    // Find a slot with a matching gpid
    if (gpid != null && !gpid.isEmpty()) {
      final SlotElement element = goodSlots.get(gpid);
      if (element != null) {
        newPiece =  element.createPiece(oldPiece, this);
        copyState(oldPiece, newPiece);
        return newPiece;
      }
    }

    // Failed to find a slot by gpid, try by matching piece name if option selected
    if (useName()) {
      final String oldPieceName = Decorator.getInnermost(oldPiece).getName();
      for (final SlotElement element : goodSlots.values()) {
        final GamePiece slotPiece = element.getPiece();
        final String gpName = Decorator.getInnermost(slotPiece).getName();
        if (oldPieceName.equals(gpName)) {
          newPiece = element.createPiece(oldPiece, this);
          copyState(oldPiece, newPiece);
          return newPiece;
        }
      }
    }

    return oldPiece;
  }


 /* public boolean findUpdatedPiece(GamePiece oldPiece) {
    // Find a slot with a matching gpid
    final String gpid = (String) oldPiece.getProperty(Properties.PIECE_ID);
    if (gpid != null && gpid.length() > 0) {
      final SlotElement element = goodSlots.get(gpid);
      if (element != null) {
        return true;
      }
    }

    // Failed to find a slot by gpid, try by matching piece name if option selected
    if (useName()) {
      final String oldPieceName = Decorator.getInnermost(oldPiece).getName();
      for (final SlotElement el : goodSlots.values()) {
        final GamePiece newPiece = el.getPiece();
        final String newPieceName = Decorator.getInnermost(newPiece).getName();
        if (oldPieceName.equals(newPieceName)) {
          return true;
        }
      }
    }

    return false;
  }*/
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
        final Decorator decoratorNew = (Decorator) p;
        final String newState = findState(oldPiece, p, decoratorNew, p.getClass());
        // Do not copy the state of Marker traits, we want to see the new value from the new definition
        if (newState != null && newState.length() > 0 && !(decoratorNew instanceof Marker)) {
          decoratorNew.mySetState(newState);
        }
        p = decoratorNew.getInner();
      }
    }
  }


  /**
   * Locate a Decorator in the old piece that has the exact same
   * type as the new Decorator and return it's state
   *
   * @param oldPiece Old piece to search
   * @param classToFind Class to match
   * @return state of located matching Decorator
   */
  protected String findState(GamePiece oldPiece, GamePiece pNew, Decorator decoratorNewPc, Class<? extends GamePiece> classToFind) {
    GamePiece p = oldPiece;
    final String typeToFind = decoratorNewPc.myGetType();
    while (p != null && !(p instanceof BasicPiece)) {
      final Decorator d = (Decorator) Decorator.getDecorator(p, classToFind);
      if (d != null) {
        if (d.getClass().equals(classToFind)) {
          if (d.myGetType().equals(typeToFind)) {
            return d.myGetState();
          }
          else if (d instanceof Labeler) {
            if (useLabelerName()) {
              final String nameToFind = ((Labeler)decoratorNewPc).getActualDescription();
              final String name = ((Labeler)d).getActualDescription();
              if (name.equals(nameToFind))
                return d.myGetState();
            }
          }
          else if (d instanceof Embellishment) {
            if (useLayerName()) {
              final String nameToFind = ((Embellishment)decoratorNewPc).getLayerName();
              if (((Embellishment) d).getLayerName().equals(nameToFind)) {
                return d.myGetState();
              }
            }
          }

        }
        p = d.getInner();
      }
      else
        p = null;
    }
    return null;
  }


  /**
   * Wrapper class for components that contain a GpId - They will all be either
   * PieceSlot components or PlaceMarker Decorator's.
   * PlaceMarker's may exist inside Prototypes and require special handling
   * Ideally we would add an interface to these components, but this
   * will break any custom code based on PlaceMarker
   *
   */
  static class SlotElement {

    private PieceSlot slot;
    private PlaceMarker marker;
    private String id;
    private PrototypeDefinition prototype;
    private GamePiece expandedPrototype;
    private GpIdChecker gpIdChecker;

    public SlotElement() {
      slot = null;
      marker = null;
      prototype = null;
      expandedPrototype = null;
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

    public SlotElement(PlaceMarker pm, PrototypeDefinition pd, GamePiece definition) {
      this();
      marker = pm;
      prototype = pd;
      expandedPrototype = definition;
      id = pm.getGpId();
    }

    public String getGpId() {
      return id;
    }

    @Override
    public String toString() {
      return marker == null ? Resources.getString("GpIdChecker.piece_slot", slot.getConfigureName()) : Resources.getString("GpIdChecker.place_replace_trait", marker.getDescription());
    }

    public void updateGpId() {
      if (marker == null) {
        slot.updateGpId();
        id = slot.getGpId();
      }
      else {
        marker.updateGpId();
        id = marker.getGpId();
        // If this PlaceMarker trait lives in a Prototype, then the Prototype definition has to be updated
        if (prototype != null) {
          prototype.setPiece(expandedPrototype);
        }
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
     * Create a new GamePiece based on this Slot Element.
     * State information contained in the OldPiece is transferred to
     * the new piece
     *
     * @param oldPiece Old Piece for state information
     * @return New Piece
     */
    public GamePiece createPiece(GamePiece oldPiece, GpIdChecker gpIdChecker) {
      this.gpIdChecker =  gpIdChecker;
      GamePiece newPiece = (slot != null) ? slot.getPiece() : marker.createMarker();
      // The following two steps create a complete new GamePiece with all
      // prototypes expanded
      newPiece = PieceCloner.getInstance().clonePiece(newPiece);
      // copyState(oldPiece, newPiece); move up to allow for new piece buffering
      newPiece.setProperty(Properties.PIECE_ID, getGpId());
      return newPiece;
    }
  }
}