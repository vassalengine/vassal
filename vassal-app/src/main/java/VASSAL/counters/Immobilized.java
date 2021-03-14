/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.counters;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.swing.KeyStroke;

/**
 * Decorator that filters events to prevent a GamePiece from
 * being selected and/or moved.
 *
 * Note: The Alt selection filter was originally implemented
 * as a ctl-shift filter, but this conflicts with the standard counter
 * selection interface and has not worked since v3.0.
 *
 */
public class Immobilized extends Decorator implements EditablePiece {

  public static final String ID = "immob;"; // NON-NLS
  protected boolean shiftToSelect = false;
  protected boolean altToSelect = false;
  protected boolean ignoreGrid = false;
  protected boolean neverSelect = false;
  protected boolean neverMove = false;
  protected boolean moveIfSelected = false;
  protected boolean neverBandSelect = false;
  protected boolean altToBandSelect = false;
  protected boolean altShiftToBandSelect = false;
  protected EventFilter selectFilter;
  protected EventFilter moveFilter;
  protected EventFilter bandselectFilter;

  protected static final char MOVE_SELECTED = 'I';
  protected static final char MOVE_NORMAL = 'N';
  protected static final char NEVER_MOVE = 'V';
  protected static final char IGNORE_GRID = 'g';
  protected static final char SHIFT_SELECT = 'i';
  protected static final char ALT_SELECT = 'c'; //NB. Using 'c' to maintain compatibility with old ctl-shift version
  protected static final char NEVER_SELECT = 'n';
  protected static final char NEVER_BAND_SELECT = 'Z';
  protected static final char ALT_BAND_SELECT = 'A';
  protected static final char ALT_SHIFT_BAND_SELECT = 'B';

  public class UseShift implements EventFilter {
    @Override
    public boolean rejectEvent(InputEvent evt) {
      return !evt.isShiftDown() && !Boolean.TRUE.equals(getProperty(Properties.SELECTED));
    }
  }

  public class UseAlt implements EventFilter {
    @Override
    public boolean rejectEvent(InputEvent evt) {
      return !evt.isAltDown() && !Boolean.TRUE.equals(getProperty(Properties.SELECTED));
    }
  }

  public class UseAltShift implements EventFilter {
    @Override
    public boolean rejectEvent(InputEvent evt) {
      return (!evt.isAltDown() || !evt.isShiftDown()) && !Boolean.TRUE.equals(getProperty(Properties.SELECTED));
    }
  }

  protected class MoveIfSelected implements EventFilter {
    @Override
    public boolean rejectEvent(InputEvent evt) {
      return !Boolean.TRUE.equals(getProperty(Properties.SELECTED));
    }
  }

  protected static final EventFilter NEVER = evt -> true;

  public Immobilized() {
    this(Immobilized.ID, null);
  }

  /** @deprecated Use {@link #Immobilized(String, GamePiece)} instead. */
  @Deprecated
  public Immobilized(GamePiece p, String type) {
    this(type, p);
  }

  public Immobilized(String type, GamePiece p) {
    setInner(p);
    mySetType(type);
  }

  @Override
  public void mySetType(String type) {
    shiftToSelect = false;
    altToSelect = false;
    neverSelect = false;
    ignoreGrid = false;
    neverMove = false;
    moveIfSelected = false;
    neverBandSelect = false;
    altToBandSelect = false;
    altShiftToBandSelect = false;
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    final String selectionOptions = st.nextToken("");
    final String movementOptions = st.nextToken("");
    if (selectionOptions.indexOf(SHIFT_SELECT) >= 0) {
      shiftToSelect = true;
      moveIfSelected = true;
    }
    if (selectionOptions.indexOf(ALT_SELECT) >= 0) {
      altToSelect = true;
      moveIfSelected = true;
    }
    if (selectionOptions.indexOf(NEVER_SELECT) >= 0) {
      neverSelect = true;
      neverMove = true;
    }
    if (selectionOptions.indexOf(IGNORE_GRID) >= 0) {
      ignoreGrid = true;
    }
    if (selectionOptions.indexOf(ALT_BAND_SELECT) >= 0) {
      altToBandSelect = true;
    }
    if (selectionOptions.indexOf(ALT_SHIFT_BAND_SELECT) >= 0) {
      altShiftToBandSelect = true;
    }
    if (selectionOptions.indexOf(NEVER_BAND_SELECT) >= 0) {
      neverBandSelect = true;
    }
    if (movementOptions.length() > 0) {
      switch (movementOptions.charAt(0)) {
      case NEVER_MOVE:
        neverMove = true;
        moveIfSelected = false;
        break;
      case MOVE_SELECTED:
        neverMove = false;
        moveIfSelected = true;
        break;
      default :
        neverMove = false;
        moveIfSelected = false;
      }
    }
    if (neverSelect) {
      selectFilter = NEVER;
    }
    else if (shiftToSelect) {
      selectFilter = new UseShift();
    }
    else if (altToSelect) {
      selectFilter = new UseAlt();
    }
    else {
      selectFilter = null;
    }
    if (neverMove) {
      moveFilter = NEVER;
    }
    else if (moveIfSelected) {
      moveFilter = new MoveIfSelected();
    }
    else {
      moveFilter = null;
    }

    if (neverBandSelect) {
      bandselectFilter = NEVER;
    }
    else if (altToBandSelect) {
      bandselectFilter = new UseAlt();
    }
    else if (altShiftToBandSelect) {
      bandselectFilter = new UseAltShift();
    }
    else {
      bandselectFilter = null;
    }
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public Command myKeyEvent(KeyStroke e) {
    return null;
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (Properties.NO_STACK.equals(key)) {
      return Boolean.TRUE;
    }
    else if (Properties.TERRAIN.equals(key)) {
      return moveIfSelected || neverMove;
    }
    else if (Properties.IGNORE_GRID.equals(key)) {
      return ignoreGrid;
    }
    else if (Properties.SELECT_EVENT_FILTER.equals(key)) {
      return selectFilter;
    }
    else if (Properties.MOVE_EVENT_FILTER.equals(key)) {
      return moveFilter;
    }
    else if (Properties.NON_MOVABLE.equals(key)) {
      return neverMove;
    }
    else if (Properties.BAND_SELECT_EVENT_FILTER.equals(key)) {
      return bandselectFilter;
    }
    else {
      return super.getLocalizedProperty(key);
    }
  }

  @Override
  public Object getProperty(Object key) {
    if (Properties.NO_STACK.equals(key)) {
      return Boolean.TRUE;
    }
    else if (Properties.TERRAIN.equals(key)) {
      return moveIfSelected || neverMove;
    }
    else if (Properties.IGNORE_GRID.equals(key)) {
      return ignoreGrid;
    }
    else if (Properties.SELECT_EVENT_FILTER.equals(key)) {
      return selectFilter;
    }
    else if (Properties.MOVE_EVENT_FILTER.equals(key)) {
      return moveFilter;
    }
    else if (Properties.NON_MOVABLE.equals(key)) {
      return neverMove;
    }
    else if (Properties.BAND_SELECT_EVENT_FILTER.equals(key)) {
      return bandselectFilter;
    }
    else {
      return super.getProperty(key);
    }
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String myGetType() {
    final StringBuilder buffer = new StringBuilder(ID);
    if (neverSelect) {
      buffer.append(NEVER_SELECT);
    }
    else if (shiftToSelect) {
      buffer.append(SHIFT_SELECT);
    }
    else if (altToSelect) {
      buffer.append(ALT_SELECT);
    }
    if (ignoreGrid) {
      buffer.append(IGNORE_GRID);
    }
    if (neverBandSelect) {
      buffer.append(NEVER_BAND_SELECT);
    }
    else if (altToBandSelect) {
      buffer.append(ALT_BAND_SELECT);
    }
    else if (altShiftToBandSelect) {
      buffer.append(ALT_SHIFT_BAND_SELECT);
    }

    buffer.append(';');
    if (neverMove) {
      buffer.append(NEVER_MOVE);
    }
    else if (moveIfSelected) {
      buffer.append(MOVE_SELECTED);
    }
    else {
      buffer.append(MOVE_NORMAL);
    }
    return buffer.toString();
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public void mySetState(String s) {
  }

  @Override
  public String getDescription() {
    return Resources.getString("Editor.Immobilized.trait_description");
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("NonStacking.html"); // NON-NLS
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    l.add(Properties.TERRAIN);
    l.add(Properties.IGNORE_GRID);
    l.add(Properties.NON_MOVABLE);
    return l;
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Immobilized)) return false;
    final Immobilized c = (Immobilized) o;

    if (! Objects.equals(shiftToSelect, c.shiftToSelect)) return false;
    if (! Objects.equals(altToSelect, c.altToSelect)) return false;
    if (! Objects.equals(neverSelect, c.neverSelect)) return false;
    if (! Objects.equals(ignoreGrid, c.ignoreGrid)) return false;
    if (! Objects.equals(neverMove, c.neverMove)) return false;
    if (! Objects.equals(moveIfSelected, c.moveIfSelected)) return false;
    if (! Objects.equals(neverBandSelect, c.neverBandSelect)) return false;
    if (! Objects.equals(altShiftToBandSelect, c.altShiftToBandSelect)) return false;

    return Objects.equals(altToBandSelect, c.altToBandSelect);
  }

  private static class Ed implements PieceEditor {
    private final TranslatingStringEnumConfigurer selectionOption;
    private final TranslatingStringEnumConfigurer movementOption;
    private final TranslatingStringEnumConfigurer bandSelectOption;
    private final BooleanConfigurer ignoreGridBox;
    private final TraitConfigPanel controls;

    private static final String NORMAL = "normally"; // NON-NLS
    private static final String SHIFT = "when shift-key down"; // NON-NLS
    private static final String ALT = "when alt-key down"; // NON-NLS
    private static final String ALT_SHIFT = "when alt+shift keys down"; // NON-NLS
    private static final String NEVER = "never"; // NON-NLS
    private static final String SELECTED = "only if selected"; // NON-NLS

    private static final String[] SELECT_OPTIONS = { NORMAL, SHIFT, ALT, NEVER}; // NON-NLS

    private static final String[] SELECT_KEYS = {
      "Editor.Immobilized.normally",
      "Editor.Immobilized.when_shift_key_down",
      "Editor.Immobilized.when_alt_key_down",
      "Editor.Immobilized.never"
    };

    private static final String[] BAND_SELECT_OPTIONS = { NORMAL, ALT, ALT_SHIFT, NEVER}; // NON-NLS

    private static final String[] BAND_SELECT_KEYS = {
      "Editor.Immobilized.normally",
      "Editor.Immobilized.when_alt_key_down",
      "Editor.Immobilized.when_alt_shift_keys_down",
      "Editor.Immobilized.never"
    };

    private static final String[] MOVE_OPTIONS = { NORMAL, SELECTED, NEVER }; // NON-NLS

    private static final String[] MOVE_KEYS = {
      "Editor.Immobilized.normally",
      "Editor.Immobilized.only_if_selected",
      "Editor.Immobilized.never"
    };

    public Ed(Immobilized p) {
      controls = new TraitConfigPanel();

      selectionOption = new TranslatingStringEnumConfigurer(SELECT_OPTIONS, SELECT_KEYS);
      if (p.neverSelect) {
        selectionOption.setValue(NEVER);
      }
      else if (p.altToSelect) {
        selectionOption.setValue(ALT);
      }
      else if (p.shiftToSelect) {
        selectionOption.setValue(SHIFT);
      }
      else {
        selectionOption.setValue(NORMAL);
      }
      controls.add("Editor.Immobilized.select_piece", selectionOption);

      bandSelectOption = new TranslatingStringEnumConfigurer(BAND_SELECT_OPTIONS, BAND_SELECT_KEYS);
      if (p.neverBandSelect) {
        bandSelectOption.setValue(NEVER);
      }
      else if (p.altToBandSelect) {
        bandSelectOption.setValue(ALT);
      }
      else if (p.altShiftToBandSelect) {
        bandSelectOption.setValue(ALT_SHIFT);
      }
      else {
        bandSelectOption.setValue(NORMAL);
      }
      controls.add("Editor.Immobilized.band_select_piece", bandSelectOption);

      movementOption = new TranslatingStringEnumConfigurer(MOVE_OPTIONS, MOVE_KEYS);
      if (p.neverMove) {
        movementOption.setValue(NEVER);
      }
      else if (p.moveIfSelected) {
        movementOption.setValue(SELECTED);
      }
      else {
        movementOption.setValue(NORMAL);
      }
      controls.add("Editor.Immobilized.move_piece", movementOption);

      ignoreGridBox = new BooleanConfigurer(p.ignoreGrid);
      controls.add("Editor.Immobilized.ignore_map_grid_when_moving", ignoreGridBox);
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      String s = ID;
      switch (selectionOption.getValueString()) {
      case SHIFT:
        s += SHIFT_SELECT;
        break;
      case ALT:
        s += ALT_SELECT;
        break;
      case NEVER:
        s += NEVER_SELECT;
      }
      if (ignoreGridBox.getValueBoolean()) {
        s += IGNORE_GRID;
      }
      switch (bandSelectOption.getValueString()) {
      case ALT:
        s += ALT_BAND_SELECT;
        break;
      case ALT_SHIFT:
        s += ALT_SHIFT_BAND_SELECT;
        break;
      case NEVER:
        s += NEVER_BAND_SELECT;
        break;
      }
      s += ';';
      switch (movementOption.getValueString()) {
      case NORMAL:
        s += MOVE_NORMAL;
        break;
      case SELECTED:
        s += MOVE_SELECTED;
        break;
      case NEVER:
        s += NEVER_MOVE;
        break;
      }
      return s;
    }

    @Override
    public Component getControls() {
      return controls;
    }
  }
}
