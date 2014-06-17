/*
 * $Id$
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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Window;
import java.awt.event.InputEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.NamedKeyStrokeArrayConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
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

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (command.length() > 0 && key != null) {
      final KeyCommand c =  new KeyCommand(command, key, Decorator
          .getOutermost(this), matchesFilter());
      if (getMap() == null) {
        c.setEnabled(false);
      }
      return new KeyCommand[] { c };
    }
    else {
      return new KeyCommand[0];
    }
  }

  public String myGetState() {
    return ""; //$NON-NLS-1$
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
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
   * @param stroke
   * @return
   */
  public Command keyEvent(KeyStroke stroke) {
    Command c = piece.keyEvent(stroke);
    return c == null ? myKeyEvent(stroke) : c.append(myKeyEvent(stroke));
  }

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
        doLoopOnce(c);
      }
      catch (RecursionLimitException e) {
        RecursionLimiter.infiniteLoop(e);
      }
      return c;
    }

    // 5. Looping

    // Set up Index Property
    indexValue = parse("Index Property Start Value", indexStart, outer);
    final int step = parse ("Index Property increment value", indexStep, outer);

    // Issue the Pre-loop key
    executeKey(c, preLoopKey);

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
        doLoopOnce(c);
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
    executeKey(c, postLoopKey);

    // Report any loop exceptions
    if (loopException != null) {
      RecursionLimiter.infiniteLoop(loopException);
    }

    return c;
  }

  private int parse (String desc, FormattedString s, GamePiece outer) {
    int i = 0;
    String val = s.getText(outer, "0");
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

  public Object getProperty(Object key) {
    if (isIndex() && indexProperty.equals(key)) {
      return String.valueOf(indexValue);
    }
    return super.getProperty(key);
  }

  public Object getLocalizedProperty(Object key) {
    if (isIndex() && indexProperty.equals(key)) {
      return String.valueOf(indexValue);
    }
    return super.getLocalizedProperty(key);
  }

  protected void doLoopOnce(Command c) throws RecursionLimitException {
    try {
      RecursionLimiter.startExecution(this);
      for (int i = 0; i < actionKeys.length && getMap() != null; i++) {
        c.append(outer.keyEvent(actionKeys[i].getKeyStroke()));
      }
    }
    finally {
      RecursionLimiter.endExecution();
    }
  }

  protected void executeKey(Command c, NamedKeyStroke key) {
    if (key.isNull() || getMap() == null) {
      return;
    }

    try {
      RecursionLimiter.startExecution(this);
      c.append(outer.keyEvent(key.getKeyStroke()));
    }
    catch (RecursionLimitException e) {
      RecursionLimiter.infiniteLoop(e);
    }
    finally {
      RecursionLimiter.endExecution();
    }
  }

  protected boolean matchesFilter() {
    final GamePiece outer = Decorator.getOutermost(this);
    if (!propertyMatch.isNull()) {
      if (!propertyMatch.accept(outer)) {
        return false;
      }
    }
    return true;
  }

  public void mySetState(String newState) {
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    String s = Resources.getString("Editor.TriggerAction.component_type"); //$NON-NLS-1$
    if (name.length() > 0) {
      s += " - " + name; //$NON-NLS-1$
    }
    return s;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("TriggerAction.htm"); //$NON-NLS-1$
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
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
        watchKeys[i] = NamedKeyStroke.getNamedKeyStroke(keys.charAt(i),
            InputEvent.CTRL_MASK);
      }
    }

    keys = st.nextToken(""); //$NON-NLS-1$
    if (keys.indexOf(',') > 0) {
      actionKeys = NamedKeyStrokeArrayConfigurer.decode(keys);
    }
    else {
      actionKeys = new NamedKeyStroke[keys.length()];
      for (int i = 0; i < actionKeys.length; i++) {
        actionKeys[i] = NamedKeyStroke.getNamedKeyStroke(keys.charAt(i),
            InputEvent.CTRL_MASK);
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
  public List<String> getPropertyNames() {
    if (isIndex()) {
      final ArrayList<String> l = new ArrayList<String>();
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

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public PieceI18nData getI18nData() {
    return getI18nData(command, getCommandDescription(name, "Trigger command")); //$NON-NLS-1$
  }

  public static class Ed implements PieceEditor {

    private StringConfigurer name;
    private StringConfigurer command;
    private NamedHotKeyConfigurer key;
    private PropertyExpressionConfigurer propertyMatch;
    private NamedKeyStrokeArrayConfigurer watchKeys;
    private NamedKeyStrokeArrayConfigurer actionKeys;
    private JPanel box;
    private BooleanConfigurer loopConfig;
    private NamedHotKeyConfigurer preLoopKeyConfig;
    private NamedHotKeyConfigurer postLoopKeyConfig;
    private StringEnumConfigurer loopTypeConfig;
    private PropertyExpressionConfigurer whileExpressionConfig;
    private PropertyExpressionConfigurer untilExpressionConfig;
    private FormattedStringConfigurer loopCountConfig;
    private BooleanConfigurer indexConfig;
    private StringConfigurer indexPropertyConfig;
    private FormattedStringConfigurer indexStartConfig;
    private FormattedStringConfigurer indexStepConfig;

    public Ed(TriggerAction piece) {

      final PropertyChangeListener updateListener = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent arg0) {
          updateVisibility();
        }
      };

      box = new JPanel();
      box.setLayout(new BoxLayout(box, BoxLayout.Y_AXIS));

      name = new StringConfigurer(null, Resources.getString("Editor.description_label"), piece.name); //$NON-NLS-1$
      box.add(name.getControls());

      propertyMatch = new PropertyExpressionConfigurer(null,
          Resources.getString("Editor.TriggerAction.trigger_when_properties"), piece.propertyMatch, Decorator //$NON-NLS-1$
              .getOutermost(piece));
      box.add(propertyMatch.getControls());

      Box commandBox = Box.createHorizontalBox();
      command = new StringConfigurer(null, Resources.getString("Editor.menu_command"), piece.command); //$NON-NLS-1$
      commandBox.add(command.getControls());
      key = new NamedHotKeyConfigurer(null, Resources.getString("Editor.keyboard_command"), piece.key); //$NON-NLS-1$
      commandBox.add(key.getControls());
      box.add(commandBox);

      watchKeys = new NamedKeyStrokeArrayConfigurer(null,
          Resources.getString("Editor.TriggerAction.watch_for"), piece.watchKeys); //$NON-NLS-1$
      box.add(watchKeys.getControls());

      actionKeys = new NamedKeyStrokeArrayConfigurer(null,
          Resources.getString("Editor.TriggerAction.perform_keystrokes"), piece.actionKeys); //$NON-NLS-1$
      box.add(actionKeys.getControls());

      loopConfig = new BooleanConfigurer(null,
          Resources.getString("Editor.TriggerAction.repeat_this"), piece.loop); //$NON-NLS-1$
      loopConfig.addPropertyChangeListener(updateListener);
      box.add(loopConfig.getControls());

      loopTypeConfig = new StringEnumConfigurer(null, Resources.getString("Editor.LoopControl.type_of_loop"), //$NON-NLS-1$
          LoopControl.LOOP_TYPE_DESCS);
      loopTypeConfig.setValue(LoopControl.loopTypeToDesc(piece.loopType));

      loopTypeConfig.addPropertyChangeListener(updateListener);
      box.add(loopTypeConfig.getControls());

      loopCountConfig = new FormattedExpressionConfigurer(null,
          Resources.getString("Editor.LoopControl.loop_how_many"), piece.loopCount.getFormat(), piece); //$NON-NLS-1$
      box.add(loopCountConfig.getControls());

      whileExpressionConfig = new PropertyExpressionConfigurer(null,
          Resources.getString("Editor.TriggerAction.looping_continues"), piece.whileExpression); //$NON-NLS-1$
      box.add(whileExpressionConfig.getControls());

      untilExpressionConfig = new PropertyExpressionConfigurer(null,
          Resources.getString("Editor.TriggerAction.looping_ends"), piece.untilExpression); //$NON-NLS-1$
      box.add(untilExpressionConfig.getControls());

      preLoopKeyConfig = new NamedHotKeyConfigurer(null,
          Resources.getString("Editor.TriggerAction.keystroke_before"), //$NON-NLS-1$
          piece.preLoopKey);
      box.add(preLoopKeyConfig.getControls());

      postLoopKeyConfig = new NamedHotKeyConfigurer(null,
          Resources.getString("Editor.TriggerAction.keystroke_after"), //$NON-NLS-1$
          piece.postLoopKey);
      box.add(postLoopKeyConfig.getControls());

      indexConfig = new BooleanConfigurer(null,
          Resources.getString("Editor.LoopControl.loop_index"), piece.index); //$NON-NLS-1$
      indexConfig.addPropertyChangeListener(updateListener);
      box.add(indexConfig.getControls());

      indexPropertyConfig = new StringConfigurer(null,
          Resources.getString("Editor.LoopControl.index_name"), piece.indexProperty); //$NON-NLS-1$
      box.add(indexPropertyConfig.getControls());

      indexStartConfig = new FormattedExpressionConfigurer(null,
          Resources.getString("Editor.LoopControl.index_start"), piece.indexStart.getFormat(), piece); //$NON-NLS-1$
      box.add(indexStartConfig.getControls());

      indexStepConfig = new FormattedExpressionConfigurer(null,
          Resources.getString("Editor.LoopControl.index_step"), piece.indexStep.getFormat(), piece); //$NON-NLS-1$
      box.add(indexStepConfig.getControls());

      updateVisibility();
    }

    private void updateVisibility() {
      final boolean isLoop = loopConfig.booleanValue().booleanValue();
      final boolean isIndex = indexConfig.booleanValue().booleanValue();
      final String type = LoopControl.loopDescToType(loopTypeConfig.getValueString());

      loopTypeConfig.getControls().setVisible(isLoop);
      loopCountConfig.getControls().setVisible(
          isLoop && type.equals(LoopControl.LOOP_COUNTED));
      whileExpressionConfig.getControls().setVisible(
          isLoop && type.equals(LoopControl.LOOP_WHILE));
      untilExpressionConfig.getControls().setVisible(
          isLoop && type.equals(LoopControl.LOOP_UNTIL));
      preLoopKeyConfig.getControls().setVisible(isLoop);
      postLoopKeyConfig.getControls().setVisible(isLoop);
      indexConfig.getControls().setVisible(isLoop);
      indexPropertyConfig.getControls().setVisible(isLoop && isIndex);
      indexStartConfig.getControls().setVisible(isLoop && isIndex);
      indexStepConfig.getControls().setVisible(isLoop && isIndex);

      Window w = SwingUtilities.getWindowAncestor(box);
      if (w != null) {
        w.pack();
      }
    }

    public Component getControls() {
      return box;
    }

    public String getState() {
      return ""; //$NON-NLS-1$
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
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
  public String getComponentName() {
    // Use inner name to prevent recursive looping when reporting errors.
    return piece.getName();
  }

  public String getComponentTypeName() {
    return getDescription();
  }
}
