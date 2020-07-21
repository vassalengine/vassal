/*
 * $Id: Embellishment0.java 8282 2012-08-21 00:05:59Z swampwallaby $
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
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
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
import javax.swing.event.ListSelectionListener;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.KeyModifiersConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.ScaledImagePainter;

/**
 *
 * Embellishment has been extensively re-written for Vassal 3.2 changing
 * both the behavior and the visual look of the configurer. A version
 * number has been added to distinguish between the two versions.
 * Note, there is also a much older Embellishment trait with a type of emb
 *
 * When editing a module, the old versions will be automatically converted to
 * the new version. One feature (multiple keystrokes for a command) cannot
 * be converted.
 *
 * This class contains the complete code of the original version 0
 * emb2 Embellishment to support editing of unconverted version 0 Embellishments
 * and run-time support for modules containing unconverted traits.
 *
 * This is essentially the latest 3.1 version of Embellishment
 */
public class Embellishment0 extends Decorator implements TranslatablePiece {
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
  protected KeyStroke resetKey;

  protected boolean followProperty;
  protected String propertyName = "";
  protected int firstLevelValue;

  // random layers
  // protected KeyCommand rndCommand;
  protected KeyStroke rndKey;
  private String rndText = "";
  // end random layers

  // Index of the image to draw. Negative if inactive. 0 is not a valid value.
  protected int value = -1;

  protected String activationStatus = "";
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

  public Embellishment0() {
    this(ID + "Activate", null);
  }

  public Embellishment0(String type, GamePiece d) {
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

  @Override
  public void mySetType(String s) {
    if (!s.startsWith(ID)) {
      originalSetType(s);
    }
    else {
      s = s.substring(ID.length());
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
      activateCommand = st.nextToken("Activate");
      activateModifiers = st.nextInt(InputEvent.CTRL_DOWN_MASK);
      activateKey = st.nextToken("A");
      upCommand = st.nextToken("Increase");
      upModifiers = st.nextInt(InputEvent.CTRL_DOWN_MASK);
      upKey = st.nextToken("]");
      downCommand = st.nextToken("Decrease");
      downModifiers = st.nextInt(InputEvent.CTRL_DOWN_MASK);
      downKey = st.nextToken("[");
      resetCommand = st.nextToken("Reset");
      resetKey = st.nextKeyStroke('R');
      resetLevel = new FormattedString(st.nextToken("1"));
      drawUnderneathWhenSelected = st.nextBoolean(false);
      xOff = st.nextInt(0);
      yOff = st.nextInt(0);
      imageName = st.nextStringArray(0);
      commonName = st.nextStringArray(imageName.length);
      loopLevels = st.nextBoolean(true);
      name = st.nextToken("");

      // random layers
      rndKey = st.nextKeyStroke(null);
      rndText = st.nextToken("");
      // end random layers

      // Follow property value
      followProperty = st.nextBoolean(false);
      propertyName = st.nextToken("");
      firstLevelValue = st.nextInt(1);

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
    activateModifiers = InputEvent.CTRL_DOWN_MASK;
    if (st2.hasMoreTokens()) {
      resetCommand = st2.nextToken();
      resetKey = st2.nextKeyStroke(null);
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

  @Override
  public void mySetState(String s) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    value = st.nextInt(1);
    activationStatus = st.nextToken(value < 0 ? "" : activateKey);
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
      .append(firstLevelValue);

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
    return se.append(String.valueOf(value)).append(activationStatus).getValue();
  }

  @Override
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

    final Object propertyValue = Decorator.getOutermost(this).getProperty(propertyName);
    final String val = propertyValue == null ? String.valueOf(firstLevelValue) : String.valueOf(propertyValue);

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

  @Override
  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      final ArrayList<KeyCommand> l = new ArrayList<>();
      final GamePiece outer = Decorator.getOutermost(this);
      if (activateCommand.length() > 0 && activateKey.length() > 0) {
        final KeyCommand k = new KeyCommand(activateCommand,
          KeyStroke.getKeyStroke(activateKey.charAt(0), activateModifiers),
          outer, this);
        k.setEnabled(nValues > 0);
        l.add(k);
      }
      if (upCommand.length() > 0 &&
        upKey.length() > 0 &&
        nValues > 1 &&
        !followProperty) {
        up = new KeyCommand(upCommand,
          KeyStroke.getKeyStroke(upKey.charAt(0), upModifiers), outer, this);
        l.add(up);
      }
      if (downCommand.length() > 0 &&
        downKey.length() > 0 &&
        nValues > 1 &&
        !followProperty) {
        down = new KeyCommand(downCommand,
          KeyStroke.getKeyStroke(downKey.charAt(0), downModifiers), outer, this);
        l.add(down);
      }
      if (resetKey != null && resetCommand.length() > 0 && !followProperty) {
        l.add(new KeyCommand(resetCommand, resetKey, outer, this));
      }
      // random layers
      if (rndKey != null && rndText.length() > 0 && !followProperty) {
        l.add(new KeyCommand(rndText, rndKey, outer, this));
      }
      // end random layers
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
    char strokeChar = getMatchingActivationChar(stroke);
    ChangeTracker tracker = null;
    if (strokeChar != 0 && nValues > 0) {  // Do not Activate if no levels defined
      tracker = new ChangeTracker(this);
      int index = activationStatus.indexOf(strokeChar);
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
    if (!followProperty) {
      for (int i = 0; i < upKey.length(); ++i) {
        if (KeyStroke.getKeyStroke(upKey.charAt(i), upModifiers).equals(stroke)) {
          if (tracker == null) {
            tracker = new ChangeTracker(this);
          }
          int val = Math.abs(value);
          if (++val > nValues) {
            val = loopLevels ? 1 : nValues;
          }
          value = value > 0 ? val : -val;
          break;
        }
      }
      for (int i = 0; i < downKey.length(); ++i) {
        if (KeyStroke.getKeyStroke(downKey.charAt(i), downModifiers).equals(stroke)) {
          if (tracker == null) {
            tracker = new ChangeTracker(this);
          }
          int val = Math.abs(value);
          if (--val < 1) {
            val = loopLevels ? nValues : 1;
          }
          value = value > 0 ? val : -val;
          break;
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
      String s = String.valueOf(super.getProperty(key));
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

  private static class Ed implements PieceEditor {
    private MultiImagePicker images;
    private JTextField activateKeyInput = new JTextField("A");
    private JTextField upKeyInput = new JTextField("]");
    private JTextField downKeyInput = new JTextField("[");
    private JTextField activateCommand = new JTextField("Activate");
    private KeyModifiersConfigurer activateModifiers = new KeyModifiersConfigurer(null, "  key:  ");
    private JTextField upCommand = new JTextField("Increase");
    private KeyModifiersConfigurer upModifiers = new KeyModifiersConfigurer(null, "  key:  ");
    private JTextField downCommand = new JTextField("Decrease");
    private KeyModifiersConfigurer downModifiers = new KeyModifiersConfigurer(null, "  key:  ");
    // random layers
    private JTextField rndCommand = new JTextField(8);
    // random layers
    private JTextField xOffInput = new JTextField(2);
    private JTextField yOffInput = new JTextField(2);
    private JTextField levelNameInput = new JTextField(8);
    private JRadioButton prefix = new JRadioButton("is prefix");
    private JRadioButton suffix = new JRadioButton("is suffix");
    private JCheckBox alwaysActive = new JCheckBox("Always active?");
    private JCheckBox drawUnderneath = new JCheckBox("Underneath when highlighted?");
    private JTextField resetLevel = new JTextField(2);
    private JTextField resetCommand = new JTextField(8);
    private JCheckBox loop = new JCheckBox("Loop through levels?");
    private HotKeyConfigurer resetKey = new HotKeyConfigurer(null, "  Keyboard:  ");
    private JTextField name = new JTextField(8);

    private JPanel controls;
    private List<String> names;
    private List<Integer> isPrefix;
    private static final Integer NEITHER = 0;
    private static final Integer PREFIX = 1;
    private static final Integer SUFFIX = 2;
    // random layers
    private HotKeyConfigurer rndKeyConfig;

    private BooleanConfigurer followConfig;
    private StringConfigurer propertyConfig;
    private IntConfigurer firstLevelConfig;

    private Box reset1Controls, reset2Controls;
    private Box rnd1Controls, rnd2Controls;

    public Ed(Embellishment0 e) {
      Box box;

      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      final Box nameControls = Box.createHorizontalBox();
      nameControls.add(new JLabel("Name:  "));
      nameControls.add(name);
      controls.add(nameControls);
      final JPanel p = new JPanel();
      p.setLayout(new GridLayout(5, 3));

      p.add(resetKey.getControls());
      activateCommand.setMaximumSize(activateCommand.getPreferredSize());
      p.add(activateCommand);
      p.add(activateModifiers.getControls());
      p.add(activateKeyInput);
      upCommand.setMaximumSize(upCommand.getPreferredSize());
      p.add(upCommand);
      p.add(upModifiers.getControls());
      p.add(upKeyInput);
      downCommand.setMaximumSize(downCommand.getPreferredSize());
      p.add(downCommand);
      p.add(downModifiers.getControls());
      p.add(downKeyInput);

      reset1Controls = Box.createHorizontalBox();
      reset1Controls.add(new JLabel("Reset to level:  "));
      reset1Controls.add(resetLevel);
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
      rndKeyConfig = new HotKeyConfigurer(null, "  Keyboard:  ", e.rndKey);
      p.add(rndKeyConfig.getControls());
      // end random layer

      box = Box.createVerticalBox();
      alwaysActive.addItemListener(new ItemListener() {
        @Override
        public void itemStateChanged(ItemEvent evt) {
          if (alwaysActive.isSelected()) {
            activateCommand.setText("");
            activateKeyInput.setText("");
            activateCommand.setEnabled(false);
            activateKeyInput.setEnabled(false);
          }
          else {
            activateCommand.setText("Activate");
            activateKeyInput.setText("A");
            activateCommand.setEnabled(true);
            activateKeyInput.setEnabled(true);
          }
        }
      });

      final JPanel checkBoxes = new JPanel();
      checkBoxes.setLayout(new GridLayout(3, 2));
      checkBoxes.add(alwaysActive);
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
        @Override
        public void propertyChange(PropertyChangeEvent e) {
          showHideFields();
        }
      });

      controls.add(box);

      images = new MultiImagePicker();
      controls.add(images);

      final JPanel p2 = new JPanel();
      p2.setLayout(new GridLayout(2, 2));

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
      p2.add(box);

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
      p2.add(box);

      JButton b = new JButton("Add Level");
      b.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent evt) {
          names.add(null);
          isPrefix.add(null);
          images.addEntry();
        }
      });
      p2.add(b);
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
      p2.add(b);

      controls.add(p2);

      images.getList().addListSelectionListener(new ListSelectionListener() {
        @Override
        public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
          updateLevelName();
        }
      });

      reset(e);
    }

    /*
     * Change visibility of fields depending on the Follow Property setting
     */
    protected void showHideFields() {
      boolean show = !followConfig.booleanValue();
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

    @Override
    public String getState() {
      return alwaysActive.isSelected() ? "1" : "-1";
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
        .append((KeyStroke) resetKey.getValue())
        .append(resetLevel.getText())
        .append(drawUnderneath.isSelected())
        .append(xOffInput.getText())
        .append(yOffInput.getText())
        .append(imageNames.toArray(new String[0]))
        .append(commonNames.toArray(new String[0]))
        .append(loop.isSelected())
        .append(name.getText())
        .append((KeyStroke) rndKeyConfig.getValue())
        .append(rndCommand.getText() == null ? "" :
          rndCommand.getText().trim())
        .append(followConfig.getValueString())
        .append(propertyConfig.getValueString())
        .append(firstLevelConfig.getValueString());

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
         .append(resetLevel.getText());

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

    @Override
    public Component getControls() {
      return controls;
    }

    public void reset(Embellishment0 e) {
      name.setText(e.name);
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

      alwaysActive.setSelected(e.activateKey.length() == 0);
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
      resetLevel.setText(e.resetLevel.getFormat());
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
