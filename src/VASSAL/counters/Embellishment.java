/*
 * $Id$
 *
 * Copyright (c) 2000-2009 by Brent Easton
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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.GridLayout;
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
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.KeyModifiersConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
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
  protected int firstLevelValue;

  // random layers
  // protected KeyCommand rndCommand;
  protected NamedKeyStroke rndKey;
  private String rndText = "";
  // end random layers

  // Index of the image to draw. Negative if inactive. 0 is not a valid value.
  protected int value = -1;

  protected String activationStatus = "";
  protected int nValues;
  protected int xOff, yOff;
  protected String imageName[];
  protected String commonName[];
  protected Rectangle size[];
  protected ScaledImagePainter imagePainter[];
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
  protected static final int BASE_VERSION = 0;
  protected static final int CURRENT_VERSION = 1;
  protected int version;

  // NamedKeyStroke support
  protected boolean alwaysActive;
  protected boolean activated;
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
    if (val >= nValues) {
      throw new IllegalArgumentException();
    }
    value = value > 0 ? val + 1 : -val - 1;
  }

  public void mySetType(String s) {
    if (!s.startsWith(ID)) {
      originalSetType(s);
    }
    else {
      s = s.substring(ID.length());
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
      activateCommand = st.nextToken("Activate");
      activateModifiers = st.nextInt(InputEvent.CTRL_MASK);
      activateKey = st.nextToken("A");
      upCommand = st.nextToken("Increase");
      upModifiers = st.nextInt(InputEvent.CTRL_MASK);
      upKey = st.nextToken("]");
      downCommand = st.nextToken("Decrease");
      downModifiers = st.nextInt(InputEvent.CTRL_MASK);
      downKey = st.nextToken("[");
      resetCommand = st.nextToken("Reset");
      resetKey = st.nextNamedKeyStroke('R');
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
      activateKeyStroke = st.nextNamedKeyStroke('A');
      increaseKeyStroke = st.nextNamedKeyStroke(']');
      decreaseKeyStroke = st.nextNamedKeyStroke('[');

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
    activateModifiers = InputEvent.CTRL_MASK;
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
    upModifiers = InputEvent.CTRL_MASK;

    downKey = st.nextToken().toUpperCase();
    downCommand = st.nextToken();
    downModifiers = InputEvent.CTRL_MASK;

    xOff = st.nextInt(0);
    yOff = st.nextInt(0);

    final ArrayList<String> l = new ArrayList<String>();
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
  }

  public String getLocalizedName() {
    return getName(true);
  }

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

  public void mySetState(String s) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    value = st.nextInt(1);
    activationStatus = st.nextToken(value < 0 ? "" : activateKey);
    activated = st.nextBoolean(activationStatus.length() == activateKey.length());
  }

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

  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(String.valueOf(value));
    return se.append(String.valueOf(value)).append(activationStatus).append(activated).getValue();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);

    checkPropertyLevel();

    if (!isActive()) {
      return;
    }

    final int i = value - 1;

    if (i < imagePainter.length && imagePainter[i] != null) {
      final Rectangle r = getCurrentImageBounds();
      imagePainter[i].draw(g, x + (int)(zoom*r.x), y + (int)(zoom*r.y), zoom, obs);
    }

    if (drawUnderneathWhenSelected &&
        Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
      piece.draw(g, x, y, obs, zoom);
    }
  }

  /*
   * Calculate the new level to display based on a property?
   */
  protected void checkPropertyLevel() {
    if (!followProperty || propertyName.length() == 0) return;

    final Object propertyValue =
      Decorator.getOutermost(this).getProperty(propertyName);
    final String val = propertyValue == null ?
      String.valueOf(firstLevelValue) : String.valueOf(propertyValue);

    try {
      int v = Integer.parseInt(val) - firstLevelValue + 1;
      if (v <= 0) v = 1;
      if (v > nValues) v = nValues;

      value = isActive() ? v : -v;
    }
    catch (NumberFormatException e) {
      reportDataError(this, Resources.getString("Error.non_number_error"), "followProperty["+propertyName+"]="+val, e);
    }
    return;
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      final ArrayList<KeyCommand> l = new ArrayList<KeyCommand>();
      final GamePiece outer = Decorator.getOutermost(this);

      if (activateCommand.length() > 0 && !alwaysActive) {
        KeyCommand k;
        if (version == BASE_VERSION) {
          k = new KeyCommand(activateCommand,
              KeyStroke.getKeyStroke(activateKey.charAt(0), activateModifiers),
              outer, this);
        }
        else {
          k = new KeyCommand(activateCommand, activateKeyStroke, outer, this);
        }
        k.setEnabled(nValues > 0);
        l.add(k);
      }

      if (version == BASE_VERSION) {
        if (upCommand.length() > 0 &&
            upKey.length() > 0 &&
            nValues > 1 &&
            !followProperty) {
          up = new KeyCommand(upCommand,
              KeyStroke.getKeyStroke(upKey.charAt(0), upModifiers), outer, this);
          l.add(up);
        }
      }
      else {
        if (upCommand.length() > 0 &&
            ! increaseKeyStroke.isNull() &&
            nValues > 1 &&
            !followProperty) {
          up = new KeyCommand(upCommand, increaseKeyStroke, outer, this);
          l.add(up);
        }
      }

      if (version == BASE_VERSION) {
        if (downCommand.length() > 0 &&
            downKey.length() > 0 &&
            nValues > 1 &&
            !followProperty) {
          down = new KeyCommand(downCommand,
            KeyStroke.getKeyStroke(downKey.charAt(0), downModifiers), outer, this);
          l.add(down);
        }
      }
      else {
        if (downCommand.length() > 0 &&
            ! decreaseKeyStroke.isNull() &&
            nValues > 1 &&
            !followProperty) {
          down = new KeyCommand(downCommand, decreaseKeyStroke, outer, this);
          l.add(down);
        }
      }

      if (resetKey != null && (! rndKey.isNull()) && resetCommand.length() > 0 && !followProperty) {
        l.add(new KeyCommand(resetCommand, resetKey, outer, this));
      }
      // random layers
      if (rndKey != null && (! rndKey.isNull()) && rndText.length() > 0 && !followProperty) {
        l.add(new KeyCommand(rndText, rndKey, outer, this));
      }
      // end random layers
      commands = l.toArray(new KeyCommand[l.size()]);
    }
    if (up != null) {
      up.setEnabled(loopLevels || Math.abs(value) < imageName.length);
    }
    if (down != null) {
      down.setEnabled(loopLevels || Math.abs(value) > 1);
    }
    return commands;
  }

  public Command myKeyEvent(KeyStroke stroke) {

    ChangeTracker tracker = null;

    if (version == BASE_VERSION) {
      final char strokeChar = getMatchingActivationChar(stroke);
      if (strokeChar != 0 && nValues > 0) {  // Do not Activate if no levels defined
        tracker = new ChangeTracker(this);
        final int index = activationStatus.indexOf(strokeChar);
        if (index < 0) {
          activationStatus += strokeChar;
        }
        else {
          String before = activationStatus.substring(0, index);
          String after = activationStatus.substring(index + 1);
          activationStatus = before + after;
        }
        if (activationStatus.length() == activateKey.length()) {
          value = Math.abs(value);
        }
        else {
          value = -Math.abs(value);
        }
      }
    }
    else {
      if (activateKeyStroke.equals(stroke) && nValues > 0) {
        tracker = new ChangeTracker(this);
        activated = ! activated;
        if (activated) {
          value = Math.abs(value);
        }
        else {
          value = -Math.abs(value);
        }
      }
    }

    if (!followProperty) {

      if (version == BASE_VERSION) {
        for (int i = 0; i < upKey.length(); ++i) {
          if (KeyStroke.getKeyStroke(upKey.charAt(i), upModifiers).equals(stroke)) {
            doIncrease(tracker);
          }
        }

        for (int i = 0; i < downKey.length(); ++i) {
          if (KeyStroke.getKeyStroke(downKey.charAt(i), downModifiers).equals(stroke)) {
            doDecrease(tracker);
          }
        }
      }
      else {
        if (increaseKeyStroke.equals(stroke)) {
          doIncrease(tracker);
        }
        if (decreaseKeyStroke.equals(stroke)) {
          doDecrease(tracker);
        }
      }

      if (resetKey != null && resetKey.equals(stroke)) {
        if (tracker == null) {
          tracker = new ChangeTracker(this);
        }
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
        if (tracker == null) {
          tracker = new ChangeTracker(this);
        }
        int val = 0;
        val = GameModule.getGameModule().getRNG().nextInt(nValues) + 1;
        value = value > 0 ? val : -val;
      }
    }
    // end random layers
    return tracker != null ? tracker.getChangeCommand() : null;
  }

  private char getMatchingActivationChar(KeyStroke stroke) {
    for (int i = 0; i < activateKey.length(); ++i) {
      if (stroke != null && stroke.equals(KeyStroke.getKeyStroke(activateKey.charAt(i), activateModifiers))) {
        return activateKey.charAt(i);
      }
    }
    return (char) 0;
  }

  protected void doIncrease(ChangeTracker tracker) {
    if (tracker == null) {
      tracker = new ChangeTracker(this);
    }
    int val = Math.abs(value);
    if (++val > nValues) {
      val = loopLevels ? 1 : nValues;
    }
    value = value > 0 ? val : -val;
    return;
  }

  protected void doDecrease(ChangeTracker tracker) {
    if (tracker == null) {
      tracker = new ChangeTracker(this);
    }
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
      String s = String.valueOf(super.getProperty(key));
      if (drawUnderneathWhenSelected) {
        s += getProperty(Properties.SELECTED);
      }
      return s;
    }
    return super.getProperty(key);
  }

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

  protected String strip (String s) {
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

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Layer.htm");
  }

  public PieceEditor getEditor() {
    return new Ed(this);
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

  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<String>();
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
    private JTextField activateKeyInput = new JTextField("A");
    private JTextField upKeyInput = new JTextField("]");
    private JTextField downKeyInput = new JTextField("[");
    private JTextField activateCommand = new JTextField("Activate");
    private KeyModifiersConfigurer activateModifiers = new KeyModifiersConfigurer(null, "  Key:  ");
    private JTextField upCommand = new JTextField("Increase");
    private KeyModifiersConfigurer upModifiers = new KeyModifiersConfigurer(null, "  Key:  ");
    private JTextField downCommand = new JTextField("Decrease");
    private KeyModifiersConfigurer downModifiers = new KeyModifiersConfigurer(null, "  Key:  ");
    // random layers
    private JTextField rndCommand = new JTextField(8);
    // random layers
    private JTextField xOffInput = new JTextField(2);
    private JTextField yOffInput = new JTextField(2);
    private JTextField levelNameInput = new JTextField(8);
    private JRadioButton prefix = new JRadioButton("is prefix");
    private JRadioButton suffix = new JRadioButton("is suffix");
    //private JCheckBox alwaysActive = new JCheckBox("Always active?");
    private JCheckBox drawUnderneath = new JCheckBox("Underneath when highlighted?");
    private FormattedExpressionConfigurer resetLevel = new FormattedExpressionConfigurer(null, "Reset to level:  ");
    private JTextField resetCommand = new JTextField(8);
    private JCheckBox loop = new JCheckBox("Loop through levels?");
    private NamedHotKeyConfigurer resetKey = new NamedHotKeyConfigurer(null, "  Key:  ");
    private JTextField name = new JTextField(8);

    private JPanel controls;
    private List<String> names;
    private List<Integer> isPrefix;
    private static final Integer NEITHER = 0;
    private static final Integer PREFIX = 1;
    private static final Integer SUFFIX = 2;
    // random layers
    private NamedHotKeyConfigurer rndKeyConfig;

    private BooleanConfigurer followConfig;
    private StringConfigurer propertyConfig;
    private IntConfigurer firstLevelConfig;

    private Box reset1Controls, reset2Controls;
    private Box rnd1Controls, rnd2Controls;

    private JButton up, down;
    private int version;
    private BooleanConfigurer alwaysActiveConfig;
    private NamedHotKeyConfigurer activateConfig;
    private NamedHotKeyConfigurer increaseConfig;
    private NamedHotKeyConfigurer decreaseConfig;


    public Ed(Embellishment e) {
      Box box;
      version = e.version;

      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      final Box nameControls = Box.createHorizontalBox();
      nameControls.add(new JLabel("Name:  "));
      nameControls.add(name);
      controls.add(nameControls);
      final JPanel p = new JPanel();
      p.setLayout(new GridLayout(5, 3));

      activateConfig = new NamedHotKeyConfigurer(null, "  Key:  ", e.activateKeyStroke);
      increaseConfig = new NamedHotKeyConfigurer(null, "  Key:  ", e.increaseKeyStroke);
      decreaseConfig = new NamedHotKeyConfigurer(null, "  Key:  ", e.decreaseKeyStroke);

      p.add(resetKey.getControls());

      activateCommand.setMaximumSize(activateCommand.getPreferredSize());
      p.add(activateCommand);
      if (version == BASE_VERSION) {
        p.add(activateModifiers.getControls());
        p.add(activateKeyInput);
      }
      else {
        p.add(activateConfig.getControls());
        p.add(new JLabel());
      }

      upCommand.setMaximumSize(upCommand.getPreferredSize());
      p.add(upCommand);
      if (version == BASE_VERSION) {
        p.add(upModifiers.getControls());
        p.add(upKeyInput);
      }
      else {
        p.add(increaseConfig.getControls());
        p.add(new JLabel());
      }

      downCommand.setMaximumSize(downCommand.getPreferredSize());
      p.add(downCommand);
      if (version == BASE_VERSION) {
        p.add(downModifiers.getControls());
        p.add(downKeyInput);
      }
      else {
        p.add(decreaseConfig.getControls());
        p.add(new JLabel());
      }

      reset1Controls = Box.createHorizontalBox();
      reset1Controls.add(resetLevel.getControls());
      p.add(reset1Controls);
      reset2Controls = Box.createHorizontalBox();
      reset2Controls.add(new JLabel("  Command:  "));
      reset2Controls.add(resetCommand);
      p.add(reset2Controls);
      p.add(resetKey.getControls());

      // random layer
      rnd1Controls = Box.createHorizontalBox();
      rnd1Controls.add(new JLabel("Randomize:  "));
      p.add(rnd1Controls);
      rnd2Controls = Box.createHorizontalBox();
      rnd2Controls.add(new JLabel("  Command:  "));
      rndCommand = new JTextField(12);
      rndCommand.setMaximumSize(rndCommand.getPreferredSize());
      rndCommand.setText(e.rndText);
      rnd2Controls.add(rndCommand);
      p.add(rnd2Controls);
      rndKeyConfig = new NamedHotKeyConfigurer(null, "  Key:  ", e.rndKey);
      p.add(rndKeyConfig.getControls());
      // end random layer

      box = Box.createVerticalBox();
      alwaysActiveConfig = new BooleanConfigurer(null, "Always active?", e.alwaysActive);
      alwaysActiveConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          updateFields();
        }
      });

      final JPanel checkBoxes = new JPanel();
      checkBoxes.setLayout(new GridLayout(3, 2));
      checkBoxes.add(alwaysActiveConfig.getControls());
      checkBoxes.add(drawUnderneath);
      checkBoxes.add(loop);
      box.add(checkBoxes);
      box.add(p);

      final Box offsetControls = Box.createHorizontalBox();
      xOffInput.setMaximumSize(xOffInput.getPreferredSize());
      xOffInput.setText("0");
      yOffInput.setMaximumSize(xOffInput.getPreferredSize());
      yOffInput.setText("0");
      offsetControls.add(new JLabel("Offset: "));
      offsetControls.add(xOffInput);
      offsetControls.add(new JLabel(","));
      offsetControls.add(yOffInput);
      checkBoxes.add(offsetControls);

      followConfig = new BooleanConfigurer(null, "Levels follow Property Value?");
      checkBoxes.add(followConfig.getControls());

      final Box levelBox = Box.createHorizontalBox();
      propertyConfig = new StringConfigurer(null, "Property Name:  ");
      levelBox.add(propertyConfig.getControls());
      firstLevelConfig = new IntConfigurer(null, " Level 1 = ", e.firstLevelValue);
      levelBox.add(firstLevelConfig.getControls());
      checkBoxes.add(levelBox);
      followConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          showHideFields();
        }
      });

      controls.add(box);

      final JPanel pickerPanel = new JPanel();
      pickerPanel.setLayout(new BorderLayout());
      images = getImagePicker();
      images.addListSelectionListener(new ListSelectionListener() {
        public void valueChanged(ListSelectionEvent e) {
          setUpDownEnabled();
        }});
      pickerPanel.add(images, BorderLayout.CENTER);

      up = new JButton(IconFactory.getIcon("go-up", IconFamily.XSMALL));
      up.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          moveSelectedUp();
        }});

      down = new JButton(IconFactory.getIcon("go-down", IconFamily.XSMALL));
      down.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          moveSelectedDown();
        }});

      final Box upDownPanel = Box.createVerticalBox();
      upDownPanel.add(Box.createVerticalGlue());
      upDownPanel.add(up);
      upDownPanel.add(down);
      upDownPanel.add(Box.createVerticalGlue());
      pickerPanel.add(upDownPanel, BorderLayout.EAST);

      controls.add(pickerPanel);

      final JPanel p2 = new JPanel();
      p2.setLayout(new GridLayout(2, 2));

      box = Box.createHorizontalBox();
      box.add(new JLabel("Level Name:  "));
      levelNameInput.setMaximumSize(levelNameInput.getPreferredSize());
      levelNameInput.addKeyListener(new KeyAdapter() {
        public void keyReleased(KeyEvent evt) {
          changeLevelName();
        }
      });
      box.add(levelNameInput);
      p2.add(box);

      box = Box.createHorizontalBox();
      prefix.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          if (prefix.isSelected()) {
            suffix.setSelected(false);
          }
          changeLevelName();
        }
      });
      suffix.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          if (suffix.isSelected()) {
            prefix.setSelected(false);
          }
          changeLevelName();
        }
      });
      box.add(prefix);
      box.add(suffix);
      p2.add(box);

      JButton b = new JButton("Add Level");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          names.add(null);
          isPrefix.add(null);
          images.addEntry();
        }
      });
      p2.add(b);
      b = new JButton("Remove Level");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          final int index = images.getList().getSelectedIndex();
          if (index >= 0) {
            names.remove(index);
            isPrefix.remove(index);
            images.removeEntryAt(index);
          }
        }
      });
      p2.add(b);

      controls.add(p2);

      images.getList().addListSelectionListener(new ListSelectionListener() {
        public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
          updateLevelName();
        }
      });

      updateFields();

      reset(e);
    }

    protected void updateFields() {
      if (alwaysActiveConfig.getValueBoolean()) {
        activateCommand.setText("");
        activateKeyInput.setText("");
        activateCommand.setEnabled(false);
        activateKeyInput.setEnabled(false);
        activateConfig.setEnabled(false);
      }
      else {
        activateCommand.setText("Activate");
        activateKeyInput.setText("A");
        activateCommand.setEnabled(true);
        activateKeyInput.setEnabled(true);
        activateConfig.setEnabled(true);
      }
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
     * Change visibility of fields depending on the Follow Property setting
     */
    protected void showHideFields() {
      boolean show = !followConfig.booleanValue().booleanValue();
      loop.setEnabled(show);
      propertyConfig.getControls().setVisible(!show);
      firstLevelConfig.getControls().setVisible(!show);
      reset1Controls.setVisible(show);
      reset2Controls.setVisible(show);
      resetKey.getControls().setVisible(show);
      rnd1Controls.setVisible(show);
      rnd2Controls.setVisible(show);
      rndKeyConfig.getControls().setVisible(show);
      upCommand.setVisible(show);
      upModifiers.getControls().setVisible(show);
      upKeyInput.setVisible(show);
      downCommand.setVisible(show);
      downModifiers.getControls().setVisible(show);
      downKeyInput.setVisible(show);
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

    public String getState() {
      return alwaysActiveConfig.getValueBoolean() ? "1" : "-1";
    }

    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      final ArrayList<String> imageNames = new ArrayList<String>();
      final ArrayList<String> commonNames = new ArrayList<String>();
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
        // TODO use IntConfigurer
        xOffInput.setText("0");
      }

      try {
        Integer.parseInt(yOffInput.getText());
      }
      catch (NumberFormatException yNAN) {
        // TODO use IntConfigurer
        yOffInput.setText("0");
      }

      se.append(activateCommand.getText())
        .append(activateModifiers.getValueString())
        .append(activateKeyInput.getText())
        .append(upCommand.getText())
        .append(upModifiers.getValueString())
        .append(upKeyInput.getText())
        .append(downCommand.getText())
        .append(downModifiers.getValueString())
        .append(downKeyInput.getText())
        .append(resetCommand.getText())
        .append(resetKey.getValueString())
        .append(resetLevel.getValueString())
        .append(drawUnderneath.isSelected())
        .append(xOffInput.getText())
        .append(yOffInput.getText())
        .append(imageNames.toArray(new String[imageNames.size()]))
        .append(commonNames.toArray(new String[commonNames.size()]))
        .append(loop.isSelected())
        .append(name.getText())
        .append(rndKeyConfig.getValueString())
        .append(rndCommand.getText() == null ? "" :
                rndCommand.getText().trim())
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

    @Deprecated
    public String oldgetType() {
      final SequenceEncoder imageList = new SequenceEncoder(';');
      int i = 0;
      for (String imageName : images.getImageNameList()) {
        String commonName = names.get(i);
        if (names.get(i) != null && commonName != null && commonName.length() > 0) {
          SequenceEncoder sub = new SequenceEncoder(imageName, ',');
          if (PREFIX.equals(isPrefix.get(i))) {
            commonName = new SequenceEncoder(commonName, '+').append("").getValue();
          }
          else if (SUFFIX.equals(isPrefix.get(i))) {
            commonName = new SequenceEncoder("", '+').append(commonName).getValue();
          }
          else {
            commonName = new SequenceEncoder(commonName, '+').getValue();
          }
          imageList.append(sub.append(commonName).getValue());
        }
        else {
          imageList.append(imageName);
        }
        i++;
      }

      try {
        Integer.parseInt(xOffInput.getText());
      }
      catch (NumberFormatException xNAN) {
        // TODO use IntConfigurer
        xOffInput.setText("0");
      }
      try {
        Integer.parseInt(yOffInput.getText());
      }
      catch (NumberFormatException yNAN) {
        yOffInput.setText("0");
      }
      String command = activateCommand.getText();
      if (drawUnderneath.isSelected()) {
        command = "_" + command;
      }

      final SequenceEncoder se2 =
        new SequenceEncoder(activateKeyInput.getText(), ';');
      se2.append(resetCommand.getText())
         .append((KeyStroke) resetKey.getValue())
         .append(resetLevel.getValueString());

      final SequenceEncoder se = new SequenceEncoder(null, ';');
      se.append(se2.getValue())
        .append(command)
        .append(upKeyInput.getText())
        .append(upCommand.getText())
        .append(downKeyInput.getText())
        .append(downCommand.getText())
        .append(xOffInput.getText())
        .append(yOffInput.getText());

      String type = ID + se.getValue() + ';'
          + (imageList.getValue() == null ? "" : imageList.getValue());
      return type;
    }

    public Component getControls() {
      return controls;
    }

    public void reset(Embellishment e) {
      name.setText(e.name);
      names = new ArrayList<String>();
      isPrefix = new ArrayList<Integer>();
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

      alwaysActiveConfig.setValue(Boolean.valueOf(e.alwaysActive));
      drawUnderneath.setSelected(e.drawUnderneathWhenSelected);
      loop.setSelected(e.loopLevels);

      images.clear();

      activateKeyInput.setText(e.activateKey);
      activateCommand.setText(e.activateCommand);
      activateModifiers.setValue(e.activateModifiers);
      upKeyInput.setText(e.upKey);
      upCommand.setText(e.upCommand);
      upModifiers.setValue(e.upModifiers);
      downKeyInput.setText(e.downKey);
      downCommand.setText(e.downCommand);
      downModifiers.setValue(e.downModifiers);
      resetKey.setValue(e.resetKey);
      resetCommand.setText(e.resetCommand);
      resetLevel.setValue(e.resetLevel.getFormat());
      xOffInput.setText(String.valueOf(e.xOff));
      yOffInput.setText(String.valueOf(e.yOff));
      images.setImageList(e.imageName);

      followConfig.setValue(Boolean.valueOf(e.followProperty));
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
