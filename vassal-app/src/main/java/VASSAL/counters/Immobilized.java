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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.tools.SequenceEncoder;

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

  public static final String ID = "immob;";
  protected boolean shiftToSelect = false;
  protected boolean altToSelect = false;
  protected boolean ignoreGrid = false;
  protected boolean neverSelect = false;
  protected boolean neverMove = false;
  protected boolean moveIfSelected = false;
  protected boolean neverBandSelect = false; 
  protected boolean altToBandSelect = false; 
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

  protected class MoveIfSelected implements EventFilter {
    @Override
    public boolean rejectEvent(InputEvent evt) {
      return !Boolean.TRUE.equals(getProperty(Properties.SELECTED));
    }
  }

  protected static EventFilter NEVER = new EventFilter() {
    @Override
    public boolean rejectEvent(InputEvent evt) {
      return true;
    }
  };

  public Immobilized() {
    this(null, Immobilized.ID);
  }

  public Immobilized(GamePiece p, String type) {
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
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    String selectionOptions = st.nextToken("");
    String movementOptions = st.nextToken("");
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
    return new KeyCommand[0];
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
    return "Does not stack";
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("NonStacking.htm");
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
    ArrayList<String> l = new ArrayList<>();
    l.add(Properties.TERRAIN);
    l.add(Properties.IGNORE_GRID);
    l.add(Properties.NON_MOVABLE);
    return l;
  }

  private static class Ed implements PieceEditor {
    private JComboBox<String> selectionOption;
    private JComboBox<String> movementOption;
    private JComboBox<String> bandSelectOption;
    private JCheckBox ignoreGridBox;
    private Box controls;

    public Ed(Immobilized p) {
      selectionOption = new JComboBox<>();
      selectionOption.addItem("normally");
      selectionOption.addItem("when shift-key down");
      selectionOption.addItem("when alt-key down");
      selectionOption.addItem("never");
      if (p.neverSelect) {
        selectionOption.setSelectedIndex(3);
      }
      else if (p.altToSelect) {
        selectionOption.setSelectedIndex(2);
      }
      else if (p.shiftToSelect) {
        selectionOption.setSelectedIndex(1);
      }
      else {
        selectionOption.setSelectedIndex(0);
      }
      ignoreGridBox = new JCheckBox("Ignore map grid when moving?");
      ignoreGridBox.setSelected(p.ignoreGrid);
      controls = Box.createVerticalBox();
      Box b = Box.createHorizontalBox();
      b.add(new JLabel("Select piece:  "));
      b.add(selectionOption);
      controls.add(b);

      bandSelectOption = new JComboBox<>();
      bandSelectOption.addItem("normally");
      bandSelectOption.addItem("when alt-key down");
      bandSelectOption.addItem("never");
      if (p.neverBandSelect) {
        bandSelectOption.setSelectedIndex(2);
      }
      else if (p.altToBandSelect ) {
        bandSelectOption.setSelectedIndex(1);
      }
      else {
        bandSelectOption.setSelectedIndex(0);
      }      

      b = Box.createHorizontalBox();
      b.add(new JLabel("Band-Select piece:  "));
      b.add(bandSelectOption);
      controls.add(b);
            
      movementOption = new JComboBox<>();
      movementOption.addItem("normally");
      movementOption.addItem("only if selected");
      movementOption.addItem("never");
      if (p.neverMove) {
        movementOption.setSelectedIndex(2);
      }
      else if (p.moveIfSelected) {
        movementOption.setSelectedIndex(1);
      }
      else {
        movementOption.setSelectedIndex(0);
      }
      b = Box.createHorizontalBox();
      b.add(new JLabel("Move piece:  "));
      b.add(movementOption);
      controls.add(b);
      controls.add(ignoreGridBox);
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      String s = ID;
      switch (selectionOption.getSelectedIndex()) {
      case 1:
        s += SHIFT_SELECT;
        break;
      case 2:
        s += ALT_SELECT;
        break;
      case 3:
        s += NEVER_SELECT;
      }
      if (ignoreGridBox.isSelected()) {
        s += IGNORE_GRID;
      }
      switch (bandSelectOption.getSelectedIndex()) {
      case 1:
        s += ALT_BAND_SELECT;
        break;
      case 2:
        s += NEVER_BAND_SELECT;
        break;
      }
      s += ';';
      switch (movementOption.getSelectedIndex()) {
      case 0:
        s += MOVE_NORMAL;
        break;
      case 1:
        s += MOVE_SELECTED;
        break;
      case 2:
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
