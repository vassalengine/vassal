/*
 *
 * Copyright (c) 2000-2012 by Brent Easton, Rodney Kinney
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
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.geom.Area;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import net.miginfocom.swing.MigLayout;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PropertyNameExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.script.expression.Expression;
import VASSAL.script.expression.ExpressionException;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.icon.IconFamily;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.ScaledImagePainter;

/**
 * The "Layer" trait. Contains a list of images that the user may cycle through.
 * The current image is superimposed over the inner piece. The entire layer may
 * be activated or deactivated.
 *
 * Changes to support NamedKeyStrokes:
 *  - Random and reset command changed directly to Name Key Strokes.
 *  - Disentangle alwaysActive flag from length of activateKey field. Make a
 *    separate field and save in type
 *  - Add a Version field to type to enable conversion of Activate/Increase/Decrease
 *    commands. Note commands with more than 1 target keycode cannot be converted
 *  - Simplify code. Removed Version 0 (3.1) code to a separate class Embellishment0. The BasicCommandEncoder
 *    replaces this class with an Embellishment0 if Embellishment(type, inner) returns
 *    a version 0 Embellishment trait.
 */
public class Embellishment extends Decorator implements TranslatablePiece {
  public static final String OLD_ID = "emb;";
  public static final String ID = "emb2;"; // New type encoding

  public static final String IMAGE = "_Image";
  public static final String NAME = "_Name";
  public static final String LEVEL = "_Level";
  public static final String ACTIVE = "_Active";

  protected String activateKey = "";
  protected String upKey, downKey;
  protected int activateModifiers, upModifiers, downModifiers;
  protected String upCommand, downCommand, activateCommand;
  protected String resetCommand;
  protected FormattedString resetLevel = new FormattedString("1");
  protected boolean loopLevels;
  protected NamedKeyStroke resetKey;

  protected boolean followProperty;
  protected String propertyName = "";
  protected Expression followPropertyExpression;
  protected int firstLevelValue;

  // random layers
  // protected KeyCommand rndCommand;
  protected NamedKeyStroke rndKey;
  private String rndText = "";
  // end random layers

  // Index of the image to draw. Negative if inactive. 0 is not a valid value.
  protected int value = -1;

  protected int nValues;
  protected int xOff, yOff;
  protected String[] imageName;
  protected String[] commonName;
  protected Rectangle[] size;
  protected ScaledImagePainter[] imagePainter;
  protected boolean drawUnderneathWhenSelected = false;

  protected String name = "";

  protected KeyCommand[] commands;
  protected KeyCommand up = null;
  protected KeyCommand down = null;

  // Shape cache
  protected Rectangle lastBounds = null;
  protected Area lastShape = null;

  // Version control
  // Version 0 = Original multi-keystroke support for Activate/Increase/Decrease
  // Version 1 = NamedKeyStrokes for Activate/Increase/Decrease
  public static final int BASE_VERSION = 0;
  public static final int CURRENT_VERSION = 1;
  protected int version;

  // NamedKeyStroke support
  protected boolean alwaysActive;
  protected NamedKeyStroke activateKeyStroke;
  protected NamedKeyStroke increaseKeyStroke;
  protected NamedKeyStroke decreaseKeyStroke;

  public Embellishment() {
    this(ID + "Activate", null);
  }

  public Embellishment(String type, GamePiece d) {
    mySetType(type);
    setInner(d);
  }

  public boolean isActive() {
    return value > 0;
  }

  public void setActive(boolean val) {
    value = val ? Math.abs(value) : -Math.abs(value);
  }

  public int getValue() {
    return Math.abs(value) - 1;
  }

  /**
   * Set the current level - First level = 0 Does not change the active status
   *
   * @param val
   */
  public void setValue(int val) {
    int theVal = val;
    if (val >= nValues) {
      reportDataError(this, Resources.getString("Error.bad_layer"), "Layer="+val);
      theVal = nValues;
    }
    value = value > 0 ? theVal + 1 : -theVal - 1;
  }

  @Override
  public void mySetType(String s) {
    if (!s.startsWith(ID)) {
      originalSetType(s);
    }
    else {
      s = s.substring(ID.length());
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
      activateCommand = st.nextToken("");
      activateModifiers = st.nextInt(InputEvent.CTRL_DOWN_MASK);
      activateKey = st.nextToken("A");
      upCommand = st.nextToken("");
      upModifiers = st.nextInt(InputEvent.CTRL_DOWN_MASK);
      upKey = st.nextToken("");
      downCommand = st.nextToken("");
      downModifiers = st.nextInt(InputEvent.CTRL_DOWN_MASK);
      downKey = st.nextToken("");
      resetCommand = st.nextToken("");
      resetKey = st.nextNamedKeyStroke();
      resetLevel = new FormattedString(st.nextToken("1"));
      drawUnderneathWhenSelected = st.nextBoolean(false);
      xOff = st.nextInt(0);
      yOff = st.nextInt(0);
      imageName = st.nextStringArray(0);
      commonName = st.nextStringArray(imageName.length);
      loopLevels = st.nextBoolean(true);
      name = st.nextToken("");

      // random layers
      rndKey = st.nextNamedKeyStroke(null);
      rndText = st.nextToken("");
      // end random layers

      // Follow property value
      followProperty = st.nextBoolean(false);
      propertyName = st.nextToken("");
      firstLevelValue = st.nextInt(1);

      version = st.nextInt(0);
      alwaysActive = st.nextBoolean(false);
      activateKeyStroke = st.nextNamedKeyStroke();
      increaseKeyStroke = st.nextNamedKeyStroke();
      decreaseKeyStroke = st.nextNamedKeyStroke();

      // Conversion?
      if (version == BASE_VERSION) {
        alwaysActive = activateKey.length() == 0;

        // Cannot convert if activate, up or down has more than 1 char specified
        if (activateKey.length() <= 1 && upKey.length() <= 1 && downKey.length() <= 1) {
          if (activateKey.length() == 0) {
            activateKeyStroke = NamedKeyStroke.NULL_KEYSTROKE;
          }
          else {
            activateKeyStroke = new NamedKeyStroke(activateKey.charAt(0), activateModifiers);
          }

          if (upKey.length() == 0) {
            increaseKeyStroke = NamedKeyStroke.NULL_KEYSTROKE;
          }
          else {
            increaseKeyStroke = new NamedKeyStroke(upKey.charAt(0), upModifiers);
          }

          if (downKey.length() == 0) {
            decreaseKeyStroke = NamedKeyStroke.NULL_KEYSTROKE;
          }
          else {
            decreaseKeyStroke = new NamedKeyStroke(downKey.charAt(0), downModifiers);
          }
          version = CURRENT_VERSION;
        }
      }

      value = activateKey.length() > 0 ? -1 : 1;
      nValues = imageName.length;
      size = new Rectangle[imageName.length];
      imagePainter = new ScaledImagePainter[imageName.length];

      for (int i = 0; i < imageName.length; ++i) {
        imagePainter[i] = new ScaledImagePainter();
        imagePainter[i].setImageName(imageName[i]);
      }
    }

    commands = null;
  }

  /**
   * This original way of representing the type causes problems because it's not
   * extensible
   *
   * @param s
   */
  private void originalSetType(String s) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');

    st.nextToken();
    final SequenceEncoder.Decoder st2 =
      new SequenceEncoder.Decoder(st.nextToken(), ';');
    activateKey = st2.nextToken().toUpperCase();
    if (activateKey != null && activateKey.length() > 0) {
      activateKeyStroke = new NamedKeyStroke(KeyStroke.getKeyStroke(activateKey));
    }
    activateModifiers = InputEvent.CTRL_DOWN_MASK;
    if (st2.hasMoreTokens()) {
      resetCommand = st2.nextToken();
      resetKey = st2.nextNamedKeyStroke(null);
      resetLevel.setFormat(st2.nextToken("0"));
    }
    else {
      resetKey = null;
      resetCommand = "";
      resetLevel.setFormat("0");
    }

    activateCommand = st.nextToken();
    drawUnderneathWhenSelected = activateCommand.startsWith("_");
    if (drawUnderneathWhenSelected) {
      activateCommand = activateCommand.substring(1);
    }

    value = activateKey.length() > 0 ? -1 : 1;

    upKey = st.nextToken().toUpperCase();
    upCommand = st.nextToken();
    upModifiers = InputEvent.CTRL_DOWN_MASK;

    downKey = st.nextToken().toUpperCase();
    downCommand = st.nextToken();
    downModifiers = InputEvent.CTRL_DOWN_MASK;

    xOff = st.nextInt(0);
    yOff = st.nextInt(0);

    final ArrayList<String> l = new ArrayList<>();
    while (st.hasMoreTokens()) {
      l.add(st.nextToken());
    }

    nValues = l.size();
    imageName = new String[l.size()];
    commonName = new String[l.size()];
    size = new Rectangle[imageName.length];
    imagePainter = new ScaledImagePainter[imageName.length];

    for (int i = 0; i < imageName.length; ++i) {
      final String sub = l.get(i);
      final SequenceEncoder.Decoder subSt =
        new SequenceEncoder.Decoder(sub, ',');
      imageName[i] = subSt.nextToken();
      imagePainter[i] = new ScaledImagePainter();
      imagePainter[i].setImageName(imageName[i]);
      if (subSt.hasMoreTokens()) {
        commonName[i] = subSt.nextToken();
      }
    }
    loopLevels = true;

    alwaysActive = activateKey.length() == 0;
    if (activateKey.length() == 0) {
      activateKeyStroke = NamedKeyStroke.NULL_KEYSTROKE;
    }
    else {
      activateKeyStroke = new NamedKeyStroke(activateKey.charAt(0), activateModifiers);
    }

    if (upKey.length() == 0) {
      increaseKeyStroke = NamedKeyStroke.NULL_KEYSTROKE;
    }
    else {
      increaseKeyStroke = new NamedKeyStroke(upKey.charAt(0), upModifiers);
    }

    if (downKey.length() == 0) {
      decreaseKeyStroke = NamedKeyStroke.NULL_KEYSTROKE;
    }
    else {
      decreaseKeyStroke = new NamedKeyStroke(downKey.charAt(0), downModifiers);
    }
    version = CURRENT_VERSION;

  }

  @Override
  public String getLocalizedName() {
    return getName(true);
  }

  @Override
  public String getName() {
    return getName(false);
  }

  public String getName(boolean localized) {
    checkPropertyLevel(); // Name Change?
    String name = null;

    final String cname = 0 < value && value - 1 < commonName.length ?
                         getCommonName(localized, value - 1) : null;

    if (cname != null && cname.length() > 0) {
      final SequenceEncoder.Decoder st =
        new SequenceEncoder.Decoder(cname, '+');
      final String first = st.nextToken();
      if (st.hasMoreTokens()) {
        final String second = st.nextToken();
        if (first.length() == 0) {
          name = (localized ? piece.getLocalizedName() : piece.getName()) + second;
        }
        else {
          name = first + (localized ? piece.getLocalizedName() : piece.getName());
        }
      }
      else {
        name = first;
      }
    }
    else {
      name = (localized ? piece.getLocalizedName() : piece.getName());
    }

    return name;
  }

  /**
   * Return raw Embellishment name
   * @return Embellishment name
   */
  public String getLayerName() {
    return name == null ? "" : name;
  }

  @Override
  public void mySetState(String s) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    value = st.nextInt(-1);
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(activateCommand)
      .append(activateModifiers)
      .append(activateKey)
      .append(upCommand)
      .append(upModifiers)
      .append(upKey)
      .append(downCommand)
      .append(downModifiers)
      .append(downKey)
      .append(resetCommand)
      .append(resetKey)
      .append(resetLevel.getFormat())
      .append(drawUnderneathWhenSelected)
      .append(xOff)
      .append(yOff)
      .append(imageName)
      .append(commonName)
      .append(loopLevels)
      .append(name)
      .append(rndKey)   // random layers
      .append(rndText)  // random layers
      .append(followProperty)
      .append(propertyName)
      .append(firstLevelValue)
      .append(version)
      .append(alwaysActive)
      .append(activateKeyStroke)
      .append(increaseKeyStroke)
      .append(decreaseKeyStroke);

    return ID + se.getValue();
  }

  @Deprecated
  public String oldGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    final SequenceEncoder se2 = new SequenceEncoder(activateKey, ';');

    se2.append(resetCommand)
       .append(resetKey)
       .append(String.valueOf(resetLevel));

    se.append(se2.getValue())
      .append(drawUnderneathWhenSelected ?
              "_" + activateCommand : activateCommand)
      .append(upKey)
      .append(upCommand)
      .append(downKey)
      .append(downCommand)
      .append(xOff)
      .append(yOff);

    for (int i = 0; i < nValues; ++i) {
      if (commonName[i] != null) {
        SequenceEncoder sub = new SequenceEncoder(imageName[i], ',');
        se.append(sub.append(commonName[i]).getValue());
      }
      else {
        se.append(imageName[i]);
      }
    }
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');

    /*
     * Fix for Bug 9700 is to strip back the encoding of State to the simplest case.
     * Both Activation status and level is determined by the value parameter.
     */
    return se.append(String.valueOf(value)).getValue();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    final boolean drawUnder = drawUnderneathWhenSelected && Boolean.TRUE.equals(getProperty(Properties.SELECTED));

    if (!drawUnder) {
      piece.draw(g, x, y, obs, zoom);
    }

    checkPropertyLevel();

    if (!isActive()) {
      if (drawUnder) {
        piece.draw(g, x, y, obs, zoom);
      }
      return;
    }

    final int i = value - 1;

    if (i < imagePainter.length && imagePainter[i] != null) {
      final Rectangle r = getCurrentImageBounds();
      imagePainter[i].draw(g, x + (int)(zoom*r.x), y + (int)(zoom*r.y), zoom, obs);
    }

    if (drawUnder) {
      piece.draw(g, x, y, obs, zoom);
    }
  }

  /*
   * Calculate the new level to display based on a property?
   */
  protected void checkPropertyLevel() {
    if (!followProperty || propertyName.length() == 0) return;

    if (followPropertyExpression == null) {
      followPropertyExpression = Expression.createSimplePropertyExpression(propertyName);
    }

    String val = "";
    try {

      val = followPropertyExpression.evaluate(Decorator.getOutermost(this));
      if (val == null || val.length() == 0) val = String.valueOf(firstLevelValue);

      int v = Integer.parseInt(val) - firstLevelValue + 1;
      if (v <= 0) v = 1;
      if (v > nValues) v = nValues;

      value = isActive() ? v : -v;
    }
    catch (NumberFormatException e) {
      reportDataError(this, Resources.getString("Error.non_number_error"), "followProperty["+propertyName+"]="+val, e);
    }
    catch (ExpressionException e) {
      reportDataError(this, Resources.getString("Error.expression_error"), "followProperty["+propertyName+"]", e);
    }
    return;
  }

  @Override
  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      final ArrayList<KeyCommand> l = new ArrayList<>();
      final GamePiece outer = Decorator.getOutermost(this);

      if (activateCommand != null && activateCommand.length() > 0 &&
          !alwaysActive)
      {
        KeyCommand k;
        k = new KeyCommand(activateCommand, activateKeyStroke, outer, this);
        k.setEnabled(nValues > 0);
        l.add(k);
      }

      if (!followProperty) {
        if (nValues > 1) {
          if (upCommand != null && upCommand.length() > 0 &&
              increaseKeyStroke != null && !increaseKeyStroke.isNull())
          {
            up = new KeyCommand(upCommand, increaseKeyStroke, outer, this);
            l.add(up);
          }

          if (downCommand != null && downCommand.length() > 0 &&
              decreaseKeyStroke != null && !decreaseKeyStroke.isNull())
          {
            down = new KeyCommand(downCommand, decreaseKeyStroke, outer, this);
            l.add(down);
          }
        }

        if (resetKey != null && !resetKey.isNull() &&
            resetCommand.length() > 0)
        {
          l.add(new KeyCommand(resetCommand, resetKey, outer, this));
        }

        // random layers
        if (rndKey != null && !rndKey.isNull() && rndText.length() > 0) {
          l.add(new KeyCommand(rndText, rndKey, outer, this));
        }
        // end random layers
      }

      commands = l.toArray(new KeyCommand[0]);
    }

    if (up != null) {
      up.setEnabled(loopLevels || Math.abs(value) < imageName.length);
    }

    if (down != null) {
      down.setEnabled(loopLevels || Math.abs(value) > 1);
    }

    return commands;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {

    final ChangeTracker tracker = new ChangeTracker(this);

    if (activateKeyStroke.equals(stroke) && nValues > 0 && !alwaysActive) {
      value = - value;
//      activated = ! activated;
//      if (activated) {
//        value = Math.abs(value);
//      }
//      else {
//        value = -Math.abs(value);
//      }
    }

    if (!followProperty) {

      if (increaseKeyStroke.equals(stroke)) {
        doIncrease();
      }
      if (decreaseKeyStroke.equals(stroke)) {
        doDecrease();
      }

      if (resetKey != null && resetKey.equals(stroke)) {
        final GamePiece outer = Decorator.getOutermost(this);
        final String levelText = resetLevel.getText(outer);
        try {
          final int level = Integer.parseInt(levelText);
          setValue(Math.abs(level) - 1);
          setActive(level > 0);
        }
        catch (NumberFormatException e) {
          reportDataError(this, Resources.getString("Error.non_number_error"), resetLevel.debugInfo(levelText, "resetLevel"), e);
        }
      }
      // random layers
      if (rndKey != null && rndKey.equals(stroke)) {
        int val = 0;
        val = GameModule.getGameModule().getRNG().nextInt(nValues) + 1;
        value = value > 0 ? val : -val;
      }
    }
    // end random layers
    return tracker.isChanged() ? tracker.getChangeCommand() : null;
  }

  protected void doIncrease() {
    int val = Math.abs(value);
    if (++val > nValues) {
      val = loopLevels ? 1 : nValues;
    }
    value = value > 0 ? val : -val;
    return;
  }

  protected void doDecrease() {
    int val = Math.abs(value);
    if (--val < 1) {
      val = loopLevels ? nValues : 1;
    }
    value = value > 0 ? val : -val;
    return;
  }

  /** @deprecated Use {@link ImageOp.getImage} instead. */
  @Deprecated
  protected Image getCurrentImage() throws java.io.IOException {
    // nonpositive value means that layer is inactive
    // null or empty imageName[value-1] means that this layer has no image
    if (value <= 0 ||
        imageName[value-1] == null ||
        imageName[value-1].length() == 0 ||
        imagePainter[value-1] == null ||
        imagePainter[value-1].getSource() == null) return null;

    return imagePainter[value-1].getSource().getImage();
  }

  @Override
  public Rectangle boundingBox() {
    final Rectangle r = piece.boundingBox();
    if (value > 0) r.add(getCurrentImageBounds());
    return r;
  }

  public Rectangle getCurrentImageBounds() {
    if (value > 0) {
      final int i = value - 1;

      if (i >= size.length) {
        // Occurs when adding a layer with a name, but no image
        return new Rectangle();
      }

      if (size[i] == null) {
        if (imagePainter[i] != null) {
          size[i] = ImageUtils.getBounds(imagePainter[i].getImageSize());
          size[i].translate(xOff, yOff);
        }
        else {
          size[i] = new Rectangle();
        }
      }

      return size[i];
    }
    else {
      return new Rectangle();
    }
  }

  /**
   * Return the Shape of the counter by adding the shape of this layer to the shape of all inner traits.
   * Minimize generation of new Area objects.
   */
  @Override
  public Shape getShape() {
    final Shape innerShape = piece.getShape();

    if (value > 0 && !drawUnderneathWhenSelected) {
      final Rectangle r = getCurrentImageBounds();

      // If the label is completely enclosed in the current counter shape, then we can just return
      // the current shape
      if (innerShape.contains(r.x, r.y, r.width, r.height)) {
        return innerShape;
      }
      else {
        final Area a = new Area(innerShape);

        // Cache the Area object generated. Only recreate if the layer position or size has changed
        if (!r.equals(lastBounds)) {
          lastShape = new Area(r);
          lastBounds = new Rectangle(r);
        }

        a.add(lastShape);
        return a;
      }
    }
    else {
      return innerShape;
    }
  }

  @Override
  public String getDescription() {
    String displayName = name;
    if (name == null || name.length() == 0) {
      if (imageName.length > 0 &&
          imageName[0] != null &&
          imageName[0].length() > 0) {
        displayName = imageName[0];
      }
    }
    if (displayName == null || displayName.length() == 0) {
      return "Layer";
    }
    else {
      return "Layer - " + displayName;
    }
  }

  @Override
  public Object getProperty(Object key) {
    if (key.equals(name + IMAGE)) {
      checkPropertyLevel();
      if (value > 0) {
        return imageName[Math.abs(value) - 1];
      }
      else
        return "";
    }
    else if (key.equals(name + NAME)) {
      checkPropertyLevel();
      if (value > 0) {
        return strip(commonName[Math.abs(value) - 1]);
      }
      else
        return "";
    }
    else if (key.equals(name + LEVEL)) {
      checkPropertyLevel();
      return String.valueOf(value);
    }
    else if (key.equals(name + ACTIVE)) {
      return String.valueOf(isActive());
    }
    else if (key.equals(Properties.VISIBLE_STATE)) {
      checkPropertyLevel();
      String s = String.valueOf(super.getProperty(key));
      s += value;
      if (drawUnderneathWhenSelected) {
        s += getProperty(Properties.SELECTED);
      }
      return s;
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (key.equals(name + IMAGE) ||
        key.equals(name + LEVEL) ||
        key.equals(name + ACTIVE) ||
        key.equals(Properties.VISIBLE_STATE)) {
      return getProperty(key);
    }
    else if (key.equals(name + NAME)) {

      checkPropertyLevel();
      if (value > 0) {
        return strip(getLocalizedCommonName(Math.abs(value) - 1));
      }
      else
        return "";
    }
    return super.getLocalizedProperty(key);
  }

  protected String strip(String s) {
    if (s == null) {
      return null;
    }
    if (s.startsWith("+")) {
      return s.substring(1);
    }
    if (s.endsWith("+")) {
      return s.substring(0, s.length() - 1);
    }
    return s;
  }

  /** Get the name of this level (alone) */
  protected String getCommonName(boolean localized, int i) {
    return localized ? getLocalizedCommonName(i) : commonName[i];
  }

  /** Get the localized name of this level (alone) */
  protected String getLocalizedCommonName(int i) {
    final String name = commonName[i];
    if (name == null) return null;
    final String translation = getTranslation(strip(name));
    if (name.startsWith("+")) {
      return "+" + translation;
    }
    if (name.endsWith("+")) {
      return translation + "+";
    }
    return translation;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Layer.htm");
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public int getVersion() {
    return version;
  }

  /**
   * If the argument GamePiece contains a Layer whose "activate" command matches
   * the given keystroke, and whose active status matches the boolean argument,
   * return that Layer
   */
  public static Embellishment getLayerWithMatchingActivateCommand(GamePiece piece, KeyStroke stroke, boolean active) {
    for (Embellishment layer = (Embellishment) Decorator.getDecorator(piece, Embellishment.class); layer != null; layer = (Embellishment) Decorator
        .getDecorator(layer.piece, Embellishment.class)) {
      for (int i = 0; i < layer.activateKey.length(); ++i) {
        if (stroke.equals(KeyStroke.getKeyStroke(layer.activateKey.charAt(i), layer.activateModifiers))) {
          if (active && layer.isActive()) {
            return layer;
          }
          else if (!active && !layer.isActive()) {
            return layer;
          }
          break;
        }
      }
    }
    return null;
  }

  public static Embellishment getLayerWithMatchingActivateCommand(GamePiece piece, NamedKeyStroke stroke, boolean active) {
    return getLayerWithMatchingActivateCommand(piece, stroke.getKeyStroke(), active);
  }

  @Override
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<>();
    l.add(name + IMAGE);
    l.add(name + LEVEL);
    l.add(name + ACTIVE);
    l.add(name + NAME);
    return l;
  }

  /**
   * Return Property names exposed by this trait
   */
  protected static class Ed implements PieceEditor {
    private MultiImagePicker images;
    private StringConfigurer activateCommand;
    private StringConfigurer upCommand;
    private StringConfigurer downCommand;
    private StringConfigurer rndCommand;

    private JTextField xOffInput = new JTextField(2);
    private JTextField yOffInput = new JTextField(2);
    private JTextField levelNameInput = new JTextField(10);
    private JRadioButton prefix = new JRadioButton("is prefix");
    private JRadioButton suffix = new JRadioButton("is suffix");
    private JCheckBox drawUnderneath = new JCheckBox("Underneath when highlighted?");
    private FormattedExpressionConfigurer resetLevel = new FormattedExpressionConfigurer(null, "Reset to level:  ");
    private StringConfigurer resetCommand;
    private JCheckBox loop = new JCheckBox("Loop through levels?");

    private JPanel controls;
    private List<String> names;
    private List<Integer> isPrefix;
    private static final Integer NEITHER = 0;
    private static final Integer PREFIX = 1;
    private static final Integer SUFFIX = 2;

    private BooleanConfigurer followConfig;
    private PropertyNameExpressionConfigurer propertyConfig;
    private IntConfigurer firstLevelConfig;
    private StringConfigurer nameConfig;

    private JButton up, down;
    private int version;
    private BooleanConfigurer alwaysActiveConfig;
    private NamedHotKeyConfigurer activateConfig;
    private NamedHotKeyConfigurer increaseConfig;
    private NamedHotKeyConfigurer decreaseConfig;
    private NamedHotKeyConfigurer resetConfig;
    private NamedHotKeyConfigurer rndKeyConfig;

    private JLabel activateLabel;
    private JLabel increaseLabel;
    private JLabel decreaseLabel;
    private JLabel resetLabel;
    private JLabel rndLabel;

    private JLabel actionLabel;
    private JLabel menuLabel;
    private JLabel keyLabel;
    private JLabel optionLabel;

    public Ed(Embellishment e) {
      Box box;
      version = e.version;

      controls = new JPanel();
      controls.setLayout(new MigLayout("hidemode 2,fillx","[]rel[]rel[]rel[]"));

      nameConfig = new StringConfigurer(null, "Name: ", e.getName());
      controls.add(nameConfig.getControls(), "span 4,wrap,growx");

      alwaysActiveConfig = new BooleanConfigurer(null, "Always active?", e.alwaysActive);
      alwaysActiveConfig.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          showHideFields();
        }
      });

      controls.add(alwaysActiveConfig.getControls(), "span 2");
      controls.add(drawUnderneath, "span 2,wrap");
      controls.add(loop, "span 2");

      final Box offsetControls = Box.createHorizontalBox();
      xOffInput.setMaximumSize(xOffInput.getPreferredSize());
      xOffInput.setText("0");
      yOffInput.setMaximumSize(xOffInput.getPreferredSize());
      yOffInput.setText("0");
      offsetControls.add(new JLabel("Offset: "));
      offsetControls.add(xOffInput);
      offsetControls.add(new JLabel(","));
      offsetControls.add(yOffInput);
      controls.add(offsetControls, "span 2,wrap");

      followConfig = new BooleanConfigurer(null, "Levels follow expression value?");
      controls.add(followConfig.getControls(), "span 2");

      final Box levelBox = Box.createHorizontalBox();
      propertyConfig = new PropertyNameExpressionConfigurer(null, "Follow Expression:  ");
      levelBox.add(propertyConfig.getControls());
      firstLevelConfig = new IntConfigurer(null, " Level 1 = ", e.firstLevelValue);
      levelBox.add(firstLevelConfig.getControls());
      controls.add(levelBox, "span 2,wrap");

      followConfig.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent e) {
          showHideFields();
        }
      });

      actionLabel =  new JLabel("Action");
      final Font defaultFont = actionLabel.getFont();
      final Font boldFont = new Font (defaultFont.getFamily(), Font.BOLD, defaultFont.getSize());
      actionLabel.setFont(boldFont);
      controls.add(actionLabel);

      menuLabel = new JLabel("Menu Command");
      menuLabel.setFont(boldFont);
      controls.add(menuLabel, "align center");

      keyLabel = new JLabel("Key");
      keyLabel.setFont(boldFont);
      controls.add(keyLabel, "align center");

      optionLabel = new JLabel("Option");
      optionLabel.setFont(boldFont);
      controls.add(optionLabel, "align center,wrap");

      activateConfig = new NamedHotKeyConfigurer(null, "", e.activateKeyStroke);
      increaseConfig = new NamedHotKeyConfigurer(null, "", e.increaseKeyStroke);
      decreaseConfig = new NamedHotKeyConfigurer(null, "", e.decreaseKeyStroke);
      resetConfig = new NamedHotKeyConfigurer(null, "", e.resetKey);
      rndKeyConfig = new NamedHotKeyConfigurer(null, "", e.rndKey);

      activateLabel = new JLabel("Activate Layer");
      controls.add(activateLabel);
      activateCommand = new StringConfigurer(null, "", e.activateCommand);
      controls.add(activateCommand.getControls(), "align center");
      controls.add(activateConfig.getControls(), "wrap");

      increaseLabel = new JLabel("Increase Level");
      controls.add(increaseLabel);
      upCommand = new StringConfigurer(null, "", e.upCommand);
      controls.add(upCommand.getControls(), "align center");
      controls.add(increaseConfig.getControls(), "wrap");

      decreaseLabel = new JLabel("Decrease Level");
      controls.add(decreaseLabel);
      downCommand = new StringConfigurer(null, "", e.downCommand);
      controls.add(downCommand.getControls(), "align center");
      controls.add(decreaseConfig.getControls(), "wrap");

      resetLabel = new JLabel("Reset to Level");
      controls.add(resetLabel);
      resetCommand = new StringConfigurer(null, "", e.resetCommand);
      controls.add(resetCommand.getControls(), "align center");
      controls.add(resetConfig.getControls());
      controls.add(resetLevel.getControls(), "wrap");

      rndLabel = new JLabel("Randomize");
      controls.add(rndLabel);
      rndCommand = new StringConfigurer(null, "", e.rndText);
      controls.add(rndCommand.getControls(), "align center");
      controls.add(rndKeyConfig.getControls(), "wrap");

      images = getImagePicker();
      images.addListSelectionListener(new ListSelectionListener() {
        @Override
        public void valueChanged(ListSelectionEvent e) {
          setUpDownEnabled();
        }});
      controls.add(images, "span 4,split,grow");

      up = new JButton(IconFactory.getIcon("go-up", IconFamily.XSMALL));
      up.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          moveSelectedUp();
        }});

      down = new JButton(IconFactory.getIcon("go-down", IconFamily.XSMALL));
      down.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          moveSelectedDown();
        }});

      final Box upDownPanel = Box.createVerticalBox();
      upDownPanel.add(Box.createVerticalGlue());
      upDownPanel.add(up);
      upDownPanel.add(down);
      upDownPanel.add(Box.createVerticalGlue());
      controls.add(upDownPanel, "wrap");

      box = Box.createHorizontalBox();
      box.add(new JLabel("Level Name:  "));
      levelNameInput.setMaximumSize(levelNameInput.getPreferredSize());
      levelNameInput.addKeyListener(new KeyAdapter() {
        @Override
        public void keyReleased(KeyEvent evt) {
          changeLevelName();
        }
      });
      box.add(levelNameInput);
      controls.add(box, "span 2,growx");

      box = Box.createHorizontalBox();
      prefix.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent evt) {
          if (prefix.isSelected()) {
            suffix.setSelected(false);
          }
          changeLevelName();
        }
      });
      suffix.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent evt) {
          if (suffix.isSelected()) {
            prefix.setSelected(false);
          }
          changeLevelName();
        }
      });
      box.add(prefix);
      box.add(suffix);
      controls.add(box, "span 2,center,wrap");

      final JPanel buttonPanel = new JPanel(new MigLayout("ins 0","[grow 1]rel[grow 1]"));
      JButton b = new JButton("Add Level");
      b.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent evt) {
          names.add(null);
          isPrefix.add(null);
          images.addEntry();
        }
      });
      buttonPanel.add(b, "growx");

      b = new JButton("Remove Level");
      b.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent evt) {
          final int index = images.getList().getSelectedIndex();
          if (index >= 0) {
            names.remove(index);
            isPrefix.remove(index);
            images.removeEntryAt(index);
          }
        }
      });
      buttonPanel.add(b, "growx");
      controls.add(buttonPanel, "span 4,center,growx,wrap");

      images.getList().addListSelectionListener(new ListSelectionListener() {
        @Override
        public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
          updateLevelName();
        }
      });

      showHideFields();

      reset(e);
    }

    protected void moveSelectedUp() {
      final int selected = images.getList().getSelectedIndex();
      final int count = images.getList().getModel().getSize();
      if (count > 1 && selected > 0) {
        swap(selected, selected-1);
      }
    }

    protected void moveSelectedDown() {
      final int selected = images.getList().getSelectedIndex();
      final int count = images.getList().getModel().getSize();
      if (count > 1 && selected < (count-1)) {
        swap(selected, selected+1);
      }
    }

    protected void swap(int index1, int index2) {

      final String name = names.get(index1);
      names.set(index1, names.get(index2));
      names.set(index2, name);

      final Integer prefix = isPrefix.get(index1);
      isPrefix.set(index1, isPrefix.get(index2));
      isPrefix.set(index2, prefix);

      images.swap (index1, index2);
    }

    protected void  setUpDownEnabled() {
      final int selected = images.getList().getSelectedIndex();
      final int count = images.getList().getModel().getSize();
      up.setEnabled(count > 1 && selected > 0);
      down.setEnabled(count > 1 && selected < (count-1));
    }

    /*
     * Change visibility of fields depending on the Follow Property  and Always Active settings
     */
    protected void showHideFields() {
      final boolean alwaysActive = alwaysActiveConfig.getValueBoolean();
      if (alwaysActive) {
        activateLabel.setVisible(false);
        activateCommand.getControls().setVisible(false);
        activateConfig.getControls().setVisible(false);
      }
      else {
        activateLabel.setVisible(true);
        activateCommand.getControls().setVisible(true);
        activateConfig.getControls().setVisible(true);
      }

      final boolean controlled = !followConfig.booleanValue();
      loop.setEnabled(controlled);
      propertyConfig.getControls().setVisible(!controlled);
      firstLevelConfig.getControls().setVisible(!controlled);

      increaseLabel.setVisible(controlled);
      upCommand.getControls().setVisible(controlled);
      increaseConfig.getControls().setVisible(controlled);

      decreaseLabel.setVisible(controlled);
      downCommand.getControls().setVisible(controlled);
      decreaseConfig.getControls().setVisible(controlled);

      resetLabel.setVisible(controlled);
      resetCommand.getControls().setVisible(controlled);
      resetConfig.getControls().setVisible(controlled);
      resetLevel.getControls().setVisible(controlled);

      rndLabel.setVisible(controlled);
      rndCommand.getControls().setVisible(controlled);
      rndKeyConfig.getControls().setVisible(controlled);

      final boolean labelsVisible = ((!alwaysActive) || controlled);
      actionLabel.setVisible(labelsVisible);
      menuLabel.setVisible(labelsVisible);
      keyLabel.setVisible(labelsVisible);
      optionLabel.setVisible(labelsVisible);

      Decorator.repack(controls);

    }

    private void updateLevelName() {
      int index = images.getList().getSelectedIndex();
      if (index < 0) {
        levelNameInput.setText(null);
      }
      else {
        levelNameInput.setText(names.get(index));
        prefix.setSelected(PREFIX.equals(isPrefix.get(index)));
        suffix.setSelected(SUFFIX.equals(isPrefix.get(index)));
      }
    }

    private void changeLevelName() {
      int index = images.getList().getSelectedIndex();
      if (index >= 0) {
        String s = levelNameInput.getText();
        names.set(index, s);
        if (prefix.isSelected()) {
          isPrefix.set(index, PREFIX);
        }
        else if (suffix.isSelected()) {
          isPrefix.set(index, SUFFIX);
        }
        else {
          isPrefix.set(index, NEITHER);
        }
      }
      else if (index == 0) {
        names.set(index, null);
        isPrefix.set(index, NEITHER);
      }
    }

    protected MultiImagePicker getImagePicker() {
      return new MultiImagePicker();
    }

    @Override
    public String getState() {
      return alwaysActiveConfig.getValueBoolean() ? "1" : "-1";
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      final ArrayList<String> imageNames = new ArrayList<>();
      final ArrayList<String> commonNames = new ArrayList<>();
      int i = 0;
      for (String n : images.getImageNameList()) {
        imageNames.add(n);
        String commonName = names.get(i);
        if (commonName != null && commonName.length() > 0) {
          if (PREFIX.equals(isPrefix.get(i))) {
            commonName =
              new SequenceEncoder(commonName, '+').append("").getValue();
          }
          else if (SUFFIX.equals(isPrefix.get(i))) {
            commonName =
              new SequenceEncoder("", '+').append(commonName).getValue();
          }
          else {
            commonName = new SequenceEncoder(commonName, '+').getValue();
          }
        }
        commonNames.add(commonName);
        i++;
      }

      try {
        Integer.parseInt(xOffInput.getText());
      }
      catch (NumberFormatException xNAN) {
        // TODO use IntConfigurer NB Deprecated code - don't worry
        xOffInput.setText("0");
      }

      try {
        Integer.parseInt(yOffInput.getText());
      }
      catch (NumberFormatException yNAN) {
        // TODO use IntConfigurer NB Deprecated code - don't worry
        yOffInput.setText("0");
      }

      se.append(activateCommand.getValueString())
        .append("")
        .append("")
        .append(upCommand.getValueString())
        .append("")
        .append("")
        .append(downCommand.getValueString())
        .append("")
        .append("")
        .append(resetCommand.getValueString())
        .append(resetConfig.getValueString())
        .append(resetLevel.getValueString())
        .append(drawUnderneath.isSelected())
        .append(xOffInput.getText())
        .append(yOffInput.getText())
        .append(imageNames.toArray(new String[0]))
        .append(commonNames.toArray(new String[0]))
        .append(loop.isSelected())
        .append(nameConfig.getValueString())
        .append(rndKeyConfig.getValueString())
        .append(rndCommand.getValueString() == null ? "" :
                rndCommand.getValueString().trim())
        .append(followConfig.getValueString())
        .append(propertyConfig.getValueString())
        .append(firstLevelConfig.getValueString())
        .append(version)
        .append(alwaysActiveConfig.getValueString())
        .append(activateConfig.getValueString())
        .append(increaseConfig.getValueString())
        .append(decreaseConfig.getValueString());

      return ID + se.getValue();

    }

    @Override
    public Component getControls() {
      return controls;
    }

    public void reset(Embellishment e) {
      nameConfig.setValue(e.name);
      names = new ArrayList<>();
      isPrefix = new ArrayList<>();
      for (int i = 0; i < e.commonName.length; ++i) {
        String s = e.commonName[i];
        Integer is = NEITHER;
        if (s != null && s.length() > 0) {
          SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '+');
          String first = st.nextToken();
          if (st.hasMoreTokens()) {
            String second = st.nextToken();
            if (first.length() == 0) {
              s = second;
              is = SUFFIX;
            }
            else {
              s = first;
              is = PREFIX;
            }
          }
          else {
            s = first;
          }
        }
        names.add(s);
        isPrefix.add(is);
      }

      alwaysActiveConfig.setValue(e.alwaysActive);
      drawUnderneath.setSelected(e.drawUnderneathWhenSelected);
      loop.setSelected(e.loopLevels);

      images.clear();

      activateCommand.setValue(e.activateCommand);
      upCommand.setValue(e.upCommand);
      downCommand.setValue(e.downCommand);
      resetConfig.setValue(e.resetKey);
      resetCommand.setValue(e.resetCommand);
      resetLevel.setValue(e.resetLevel.getFormat());
      xOffInput.setText(String.valueOf(e.xOff));
      yOffInput.setText(String.valueOf(e.yOff));
      images.setImageList(e.imageName);

      followConfig.setValue(e.followProperty);
      propertyConfig.setValue(e.propertyName);

      // Add at least one level if none defined
      if (images.getImageNameList().isEmpty()) {
        names.add(null);
        isPrefix.add(null);
        images.addEntry();
      }

      updateLevelName();

      showHideFields();
    }
  }

  @Override
  public PieceI18nData getI18nData() {
    final PieceI18nData data = new PieceI18nData(this);
    final String prefix = name.length() > 0 ? name+": " : "";
    if (activateKey.length() > 0) {
      data.add(activateCommand, prefix + "Activate command");
    }
    if (!followProperty) {
      data.add(upCommand, prefix + "Increase command");
      data.add(downCommand, prefix + "Decrease command");
      data.add(resetCommand, prefix + "Reset command");
      data.add(rndText, prefix + "Random command");
    }
    // Strip off prefix/suffix marker
    for (int i = 0; i < commonName.length; i++) {
      data.add(strip(commonName[i]), prefix + "Level " + (i+1) + " name");
    }
    return data;
  }
}
