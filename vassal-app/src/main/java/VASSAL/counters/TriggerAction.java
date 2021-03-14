/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.NamedKeyStrokeArrayConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
import VASSAL.tools.LoopControl;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.swing.JLabel;
import javax.swing.KeyStroke;

/**
 * Macro Execute a series of Keystrokes against this same piece - Triggered by
 * own KeyCommand or list of keystrokes - Match against an optional Property
 * Filter
 * */
public class TriggerAction extends Decorator implements TranslatablePiece,
    Loopable {

  public static final String ID = "macro;"; //$NON-NLS-1$

  protected String name = ""; //$NON-NLS-1$
  protected String command = ""; //$NON-NLS-1$
  protected NamedKeyStroke key = NamedKeyStroke.NULL_KEYSTROKE;
  protected PropertyExpression propertyMatch = new PropertyExpression();
  protected NamedKeyStroke[] watchKeys = new NamedKeyStroke[0];
  protected NamedKeyStroke[] actionKeys = new NamedKeyStroke[0];
  protected boolean loop = false;
  protected NamedKeyStroke preLoopKey = NamedKeyStroke.NULL_KEYSTROKE;
  protected NamedKeyStroke postLoopKey = NamedKeyStroke.NULL_KEYSTROKE;
  protected String loopType = LoopControl.LOOP_COUNTED;
  protected PropertyExpression whileExpression = new PropertyExpression();
  protected PropertyExpression untilExpression = new PropertyExpression();
  protected FormattedString loopCount = new FormattedString("1"); //$NON-NLS-1$
  protected boolean index = false;
  protected String indexProperty = ""; //$NON-NLS-1$
  protected FormattedString indexStart = new FormattedString("1");
  protected FormattedString indexStep = new FormattedString("1");
  protected int indexValue = 0;
  protected GamePiece outer;

  public TriggerAction() {
    this(ID, null);
  }

  public TriggerAction(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (command.length() > 0 && key != null) {
      final KeyCommand c =  new KeyCommand(command, key, Decorator
          .getOutermost(this), this, matchesFilter());
      if (getMap() == null) {
        c.setEnabled(false);
      }
      return new KeyCommand[] { c };
    }
    else {
      return KeyCommand.NONE;
    }
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(name)
      .append(command)
      .append(key)
      .append(propertyMatch.getExpression())
      .append(NamedKeyStrokeArrayConfigurer.encode(watchKeys))
      .append(NamedKeyStrokeArrayConfigurer.encode(actionKeys))
      .append(loop)
      .append(preLoopKey)
      .append(postLoopKey)
      .append(loopType)
      .append(whileExpression.getExpression())
      .append(untilExpression.getExpression())
      .append(loopCount.getFormat())
      .append(index)
      .append(indexProperty)
      .append(indexStart.getFormat())
      .append(indexStep.getFormat());

    return ID + se.getValue();
  }

  /**
   * Apply key commands to inner pieces first
   *
   * @param stroke Keystroke
   * @return Generated command
   */
  @Override
  public Command keyEvent(KeyStroke stroke) {
    final Command c = piece.keyEvent(stroke);
    return c == null ? myKeyEvent(stroke) : c.append(myKeyEvent(stroke));
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    /*
     * 1. Are we interested in this key command? Is it our command key? Does it
     * match one of our watching keystrokes?
     */
    boolean seen = false;
    if (key.equals(stroke)) {
      seen = true;
    }

    for (int i = 0; i < watchKeys.length && !seen; i++) {
      if (watchKeys[i].equals(stroke)) {
        seen = true;
        break;
      }
    }

    if (!seen) {
      return null;
    }

    // 2. Check the Property Filter if it exists.
    if (!matchesFilter()) {
      return null;
    }

    // 3. Initialise
    outer = Decorator.getOutermost(this);
    Command c = new NullCommand();

    // 4. Handle non-looping case
    if (!loop) {
      try {
        c = c.append(doLoopOnce());
      }
      catch (RecursionLimitException e) {
        RecursionLimiter.infiniteLoop(e);
      }
      return c;
    }

    // 5. Looping

    // Set up Index Property
    indexValue = parse("Index Property Start Value", indexStart, outer); // NON-NLS Error onlu=y
    final int step = parse("Index Property increment value", indexStep, outer); // NON-NLS Error only

    // Issue the Pre-loop key
    c = c.append(executeKey(preLoopKey));

    // Loop

    // Set up counters for a counted loop
    int loopCounter = 0;
    int loopCountLimit = 0;
    if (LoopControl.LOOP_COUNTED.equals(loopType)) {
      loopCountLimit = loopCount.getTextAsInt(outer, Resources.getString("Editor.LoopControl.loop_count"), this); //$NON-NLS-1$
    }
    RecursionLimitException loopException = null;

    for (;;) {

      // While loop - test condition is still true before actions
      if (LoopControl.LOOP_WHILE.equals(loopType)) {
        if (!whileExpression.accept(outer)) {
          break;
        }
      }

      // Execute the actions and catch and looping. Save any
      // loop Exception to be thrown after the post-loop code
      // to ensure post-loop key is executed.
      try {
        c = c.append(doLoopOnce());
      }
      catch (RecursionLimitException ex) {
        loopException = ex;
        break;
      }

      // Until loop - test condition is not false after loop
      if (LoopControl.LOOP_UNTIL.equals(loopType)) {
        if (untilExpression.accept(outer)) {
          break;
        }
      }

      // Check for infinite looping. Save any
      // loop Exception to be thrown after the post-loop code
      // to ensure post-loop key is executed.
      if (loopCounter++ >= LoopControl.LOOP_LIMIT) {
        loopException = new RecursionLimitException(this);
        break;
      }

      // Counted loop - Check if looped enough times
      if (LoopControl.LOOP_COUNTED.equals(loopType)) {
        if (loopCounter >= loopCountLimit) {
          break;
        }
      }

      // Increment the Index Variable
      indexValue += step;

    }

    // Issue the Post-loop key
    c = c.append(executeKey(postLoopKey));

    // Report any loop exceptions
    if (loopException != null) {
      RecursionLimiter.infiniteLoop(loopException);
    }

    return c;
  }

  private int parse(String desc, FormattedString s, GamePiece outer) {
    int i = 0;
    final String val = s.getText(outer, "0");
    try {
      i = Integer.parseInt(val);
    }
    catch (NumberFormatException e) {
      reportDataError(this, Resources.getString("Error.non_number_error"), s.debugInfo(val, desc), e);
    }
    return i;
  }

  protected boolean isIndex() {
    return loop && index && indexProperty != null && indexProperty.length() > 0;
  }

  @Override
  public Object getProperty(Object key) {
    if (isIndex() && indexProperty.equals(key)) {
      return String.valueOf(indexValue);
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (isIndex() && indexProperty.equals(key)) {
      return String.valueOf(indexValue);
    }
    return super.getLocalizedProperty(key);
  }

  protected Command doLoopOnce() throws RecursionLimitException {
    Command comm = new NullCommand();
    try {
      RecursionLimiter.startExecution(this);
      for (int i = 0; i < actionKeys.length && getMap() != null; i++) {
        comm = comm.append(outer.keyEvent(actionKeys[i].getKeyStroke()));
      }
    }
    finally {
      RecursionLimiter.endExecution();
    }
    return comm;
  }

  protected Command executeKey(NamedKeyStroke key) {
    if (key.isNull() || getMap() == null) {
      return null;
    }

    Command comm = null;

    try {
      RecursionLimiter.startExecution(this);
      comm = outer.keyEvent(key.getKeyStroke());
    }
    catch (RecursionLimitException e) {
      RecursionLimiter.infiniteLoop(e);
    }
    finally {
      RecursionLimiter.endExecution();
    }
    return comm;
  }

  protected boolean matchesFilter() {
    return propertyMatch.isNull() ||
           propertyMatch.accept(Decorator.getOutermost(this));
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getDescription() {
    String s = Resources.getString("Editor.TriggerAction.component_type"); //$NON-NLS-1$
    if (name.length() > 0) {
      s += " - " + name; //$NON-NLS-1$
    }
    return s;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("TriggerAction.html"); //$NON-NLS-1$
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    name = st.nextToken(""); //$NON-NLS-1$
    command = st.nextToken("Trigger"); //$NON-NLS-1$
    key = st.nextNamedKeyStroke('T');
    propertyMatch.setExpression(st.nextToken("")); //$NON-NLS-1$

    String keys = st.nextToken(""); //$NON-NLS-1$
    if (keys.indexOf(',') > 0) {
      watchKeys = NamedKeyStrokeArrayConfigurer.decode(keys);
    }
    else {
      watchKeys = new NamedKeyStroke[keys.length()];
      for (int i = 0; i < watchKeys.length; i++) {
        watchKeys[i] = NamedKeyStroke.of(keys.charAt(i), InputEvent.CTRL_DOWN_MASK);
      }
    }

    keys = st.nextToken(""); //$NON-NLS-1$
    if (keys.indexOf(',') > 0) {
      actionKeys = NamedKeyStrokeArrayConfigurer.decode(keys);
    }
    else {
      actionKeys = new NamedKeyStroke[keys.length()];
      for (int i = 0; i < actionKeys.length; i++) {
        actionKeys[i] = NamedKeyStroke.of(keys.charAt(i), InputEvent.CTRL_DOWN_MASK);
      }
    }
    loop = st.nextBoolean(false);
    preLoopKey = st.nextNamedKeyStroke();
    postLoopKey = st.nextNamedKeyStroke();
    loopType = st.nextToken(LoopControl.LOOP_COUNTED);
    whileExpression.setExpression(st.nextToken("")); //$NON-NLS-1$
    untilExpression.setExpression(st.nextToken("")); //$NON-NLS-1$
    loopCount.setFormat(st.nextToken("")); //$NON-NLS-1$
    index = st.nextBoolean(false);
    indexProperty = st.nextToken(""); //$NON-NLS-1$
    indexStart.setFormat(st.nextToken("1"));
    indexStep.setFormat(st.nextToken("1"));
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    if (isIndex()) {
      final ArrayList<String> l = new ArrayList<>();
      l.add(indexProperty);
      return l;
    }
    else {
      return super.getPropertyNames();
    }
  }

  // Setters for JUnit testing
  public void setPropertyMatch(String s) {
    propertyMatch.setExpression(s);
  }

  public void setCommandName(String s) {
    command = s;
  }

  public void setKey(NamedKeyStroke k) {
    key = k;
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(command, getCommandDescription(name, "Trigger command")); //$NON-NLS-1$
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof TriggerAction)) return false;
    final TriggerAction c = (TriggerAction) o;

    if (! Objects.equals(command, c.command)) return false;
    if (! Objects.equals(key, c.key)) return false;
    if (! Objects.equals(propertyMatch, c.propertyMatch)) return false;
    if (! Arrays.equals(watchKeys, c.watchKeys)) return false;
    if (! Arrays.equals(actionKeys, c.actionKeys)) return false;
    if (! Objects.equals(loop, c.loop)) return false;
    if (! Objects.equals(preLoopKey, c.preLoopKey)) return false;
    if (! Objects.equals(postLoopKey, c.postLoopKey)) return false;
    if (! Objects.equals(loopType, c.loopType)) return false;
    if (! Objects.equals(whileExpression, c.whileExpression)) return false;
    if (! Objects.equals(untilExpression, c.untilExpression)) return false;
    if (! Objects.equals(loopCount, c.loopCount)) return false;
    if (! Objects.equals(index, c.index)) return false;
    if (! Objects.equals(indexProperty, c.indexProperty)) return false;
    if (! Objects.equals(indexStart, c.indexStart)) return false;

    return Objects.equals(indexStep, c.indexStep);
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer name;
    private final StringConfigurer command;
    private final NamedHotKeyConfigurer key;
    private final PropertyExpressionConfigurer propertyMatch;
    private final NamedKeyStrokeArrayConfigurer watchKeys;
    private final NamedKeyStrokeArrayConfigurer actionKeys;
    private final TraitConfigPanel box;
    private final BooleanConfigurer loopConfig;
    private final JLabel preLabel;
    private final NamedHotKeyConfigurer preLoopKeyConfig;
    private final JLabel postLabel;
    private final NamedHotKeyConfigurer postLoopKeyConfig;
    private final JLabel loopTypeLabel;
    private final TranslatingStringEnumConfigurer loopTypeConfig;
    private final JLabel whileLabel;
    private final PropertyExpressionConfigurer whileExpressionConfig;
    private final JLabel untilLabel;
    private final PropertyExpressionConfigurer untilExpressionConfig;
    private final JLabel loopCountLabel;
    private final FormattedStringConfigurer loopCountConfig;
    private final JLabel indexLabel;
    private final BooleanConfigurer indexConfig;
    private final JLabel indexPropertyLabel;
    private final StringConfigurer indexPropertyConfig;
    private final JLabel indexStartLabel;
    private final FormattedStringConfigurer indexStartConfig;
    private final JLabel indexStepLabel;
    private final FormattedStringConfigurer indexStepConfig;

    public Ed(TriggerAction piece) {

      final PropertyChangeListener updateListener = arg0 -> updateVisibility();

      box = new TraitConfigPanel();

      name = new StringConfigurer(piece.name);
      name.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", name);

      propertyMatch = new PropertyExpressionConfigurer(piece.propertyMatch, Decorator.getOutermost(piece));
      propertyMatch.setHintKey("Editor.TriggerAction.property_match_hint");
      box.add("Editor.TriggerAction.trigger_when_properties", propertyMatch);

      command = new StringConfigurer(piece.command);
      command.setHintKey("Editor.menu_command_hint");
      box.add("Editor.menu_command", command);

      key = new NamedHotKeyConfigurer(piece.key);
      box.add("Editor.keyboard_command", key);

      watchKeys = new NamedKeyStrokeArrayConfigurer(piece.watchKeys);
      box.add("Editor.TriggerAction.watch_for", watchKeys);

      actionKeys = new NamedKeyStrokeArrayConfigurer(piece.actionKeys);
      box.add("Editor.TriggerAction.perform_keystrokes", actionKeys);

      loopConfig = new BooleanConfigurer(piece.loop);
      loopConfig.addPropertyChangeListener(updateListener);
      box.add("Editor.TriggerAction.repeat_this", loopConfig);

      loopTypeLabel = new JLabel(Resources.getString("Editor.LoopControl.type_of_loop"));
      loopTypeConfig = new TranslatingStringEnumConfigurer(LoopControl.LOOP_TYPES, LoopControl.LOOP_TYPE_DESCS, true);
      loopTypeConfig.setValue(piece.loopType);
      loopTypeConfig.addPropertyChangeListener(updateListener);
      box.add(loopTypeLabel, loopTypeConfig);

      loopCountLabel = new JLabel(Resources.getString("Editor.LoopControl.loop_how_many"));
      loopCountConfig = new FormattedExpressionConfigurer(piece.loopCount.getFormat(), piece);
      loopCountConfig.setHintKey("Editor.LoopControl.loop_how_many_hint");
      box.add(loopCountLabel, loopCountConfig);

      whileLabel = new JLabel(Resources.getString("Editor.TriggerAction.looping_continues"));
      whileExpressionConfig = new PropertyExpressionConfigurer(piece.whileExpression);
      box.add(whileLabel, whileExpressionConfig);

      untilLabel = new JLabel(Resources.getString("Editor.TriggerAction.looping_ends"));
      untilExpressionConfig = new PropertyExpressionConfigurer(piece.untilExpression);
      box.add(untilLabel, untilExpressionConfig);

      preLabel = new JLabel(Resources.getString("Editor.TriggerAction.keystroke_before"));
      preLoopKeyConfig = new NamedHotKeyConfigurer(piece.preLoopKey);
      box.add(preLabel, preLoopKeyConfig);

      postLabel = new JLabel(Resources.getString("Editor.TriggerAction.keystroke_after"));
      postLoopKeyConfig = new NamedHotKeyConfigurer(piece.postLoopKey);
      box.add(postLabel, postLoopKeyConfig);

      indexLabel = new JLabel(Resources.getString("Editor.LoopControl.loop_index"));
      indexConfig = new BooleanConfigurer(piece.index);
      indexConfig.addPropertyChangeListener(updateListener);
      box.add(indexLabel, indexConfig);

      indexPropertyLabel = new JLabel(Resources.getString("Editor.LoopControl.index_name"));
      indexPropertyConfig = new StringConfigurer(piece.indexProperty);
      indexPropertyConfig.setHintKey("Editor.LoopControl.index_name_hint");
      box.add(indexPropertyLabel, indexPropertyConfig);

      indexStartLabel = new JLabel(Resources.getString("Editor.LoopControl.index_start"));
      indexStartConfig = new FormattedExpressionConfigurer(piece.indexStart.getFormat(), piece);
      box.add(indexStartLabel, indexStartConfig);

      indexStepLabel = new JLabel(Resources.getString("Editor.LoopControl.index_step"));
      indexStepConfig = new FormattedExpressionConfigurer(piece.indexStep.getFormat(), piece);
      box.add(indexStepLabel, indexStepConfig);

      updateVisibility();
    }

    private void updateVisibility() {
      final boolean isLoop = loopConfig.booleanValue();
      final boolean isIndex = indexConfig.booleanValue();
      final String type = loopTypeConfig.getValueString();

      loopTypeConfig.getControls().setVisible(isLoop);
      loopTypeLabel.setVisible(isLoop);

      loopCountConfig.getControls().setVisible(isLoop && type.equals(LoopControl.LOOP_COUNTED));
      loopCountLabel.setVisible(isLoop && type.equals(LoopControl.LOOP_COUNTED));

      whileExpressionConfig.getControls().setVisible(isLoop && type.equals(LoopControl.LOOP_WHILE));
      whileLabel.setVisible(isLoop && type.equals(LoopControl.LOOP_WHILE));

      untilExpressionConfig.getControls().setVisible(isLoop && type.equals(LoopControl.LOOP_UNTIL));
      untilLabel.setVisible(isLoop && type.equals(LoopControl.LOOP_UNTIL));

      preLoopKeyConfig.getControls().setVisible(isLoop);
      preLabel.setVisible(isLoop);

      postLoopKeyConfig.getControls().setVisible(isLoop);
      postLabel.setVisible(isLoop);

      indexConfig.getControls().setVisible(isLoop);
      indexLabel.setVisible(isLoop);

      indexPropertyConfig.getControls().setVisible(isLoop && isIndex);
      indexPropertyLabel.setVisible(isLoop && isIndex);

      indexStartConfig.getControls().setVisible(isLoop && isIndex);
      indexStartLabel.setVisible(isLoop && isIndex);

      indexStepConfig.getControls().setVisible(isLoop && isIndex);
      indexStepLabel.setVisible(isLoop && isIndex);

      repack(box);
    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getState() {
      return ""; //$NON-NLS-1$
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(name.getValueString())
        .append(command.getValueString())
        .append(key.getValueString())
        .append(propertyMatch.getValueString())
        .append(watchKeys.getValueString())
        .append(actionKeys.getValueString())
        .append(loopConfig.getValueString())
        .append(preLoopKeyConfig.getValueString())
        .append(postLoopKeyConfig.getValueString())
        .append(LoopControl.loopDescToType(loopTypeConfig.getValueString()))
        .append(whileExpressionConfig.getValueString())
        .append(untilExpressionConfig.getValueString())
        .append(loopCountConfig.getValueString())
        .append(indexConfig.getValueString())
        .append(indexPropertyConfig.getValueString())
        .append(indexStartConfig.getValueString())
        .append(indexStepConfig.getValueString());

      return ID + se.getValue();
    }
  }

  // Implement Loopable
  @Override
  public String getComponentName() {
    // Use inner name to prevent recursive looping when reporting errors.
    return piece.getName();
  }

  @Override
  public String getComponentTypeName() {
    return getDescription();
  }


  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final List<String> l = new ArrayList<>();
    l.add(propertyMatch.getExpression());
    if (loop) {
      if (LoopControl.LOOP_WHILE.equals(loopType)) {
        l.add(whileExpression.getExpression());
      }
      else if (LoopControl.LOOP_UNTIL.equals(loopType)) {
        l.add(untilExpression.getExpression());
      }
      else if (LoopControl.LOOP_COUNTED.equals(loopType)) {
        l.add(indexStart.getFormat());
        l.add(indexStep.getFormat());
      }
    }
    return l;
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    if (loop && LoopControl.LOOP_COUNTED.equals(loopType)) {
      return List.of(indexProperty);
    }
    return new ArrayList<>();
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    final List<NamedKeyStroke> l = new ArrayList<>();
    l.add(key);
    Collections.addAll(l, watchKeys);
    Collections.addAll(l, actionKeys);
    if (loop) {
      l.add(preLoopKey);
      l.add(postLoopKey);
    }
    return l;
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(command);
  }
}
