/*
 *
 * Copyright (c) 2023 by Vassalengine.org, Brian Reynolds
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
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.search.SearchTarget;
import VASSAL.tools.SequenceEncoder;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Trait to draw a colored border around a piece
 */
public class BorderOutline extends Decorator implements TranslatablePiece {
  public static final String ID = "border;"; // NON-NLS

  private String propertyName;
  private String compareMode;
  private String propertyName2;
  private String description;
  private int thickness = 2;
  private Color color = Color.RED;

  private final ColoredBorder border = new ColoredBorder();

  public BorderOutline() {
    this(ID + ";", null); // NON-NLS
  }

  public BorderOutline(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  /**
   * Comparison Modes for property match
   */
  public enum LogicalCompareMode {
    AND("AND"), //NON-NLS
    OR("OR"),   //NON-NLS
    XOR("XOR"), //NON-NLS
    NOR("NOR"); //NON-NLS

    private static final String[] KEYS = { "Editor.AND", "Editor.OR", "Editor.XOR", "Editor.NOR" };

    private final String symbol;

    LogicalCompareMode(String symbol) {
      this.symbol = symbol;
    }

    public String getSymbol() {
      return symbol;
    }

    public static BorderOutline.LogicalCompareMode whichSymbol(String symbol) {
      for (final BorderOutline.LogicalCompareMode mode : BorderOutline.LogicalCompareMode.values()) {
        if (mode.getSymbol().equals(symbol)) {
          return mode;
        }
      }
      return AND;
    }

    public static String[] getSymbols() {
      return Arrays.stream(values())
        .map(BorderOutline.LogicalCompareMode::getSymbol)
        .toArray(String[]::new);
    }

    public static String[] getKeys() {
      return KEYS;
    }
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    propertyName = st.nextToken("");
    description  = st.nextToken("");
    thickness    = st.nextInt(2);
    color        = st.nextColor(Color.RED);
    propertyName2 = st.nextToken("");
    compareMode   = st.nextToken(LogicalCompareMode.AND.toString()); //NON-NLS

    border.setColor(color);
    border.setThickness(thickness);
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(propertyName).append(description).append(thickness).append(color).append(propertyName2).append(compareMode);
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public Command myKeyEvent(javax.swing.KeyStroke stroke) {
    return null;
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public Rectangle boundingBox() {
    final Rectangle r = piece.boundingBox();
    r.add(border.boundingBox(this));
    return r;
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  protected boolean checkProperty(String name) {
    if ((name != null) && !name.isEmpty()) {
      final Object propValue = Decorator.getOutermost(this).getProperty(name);
      if (propValue == null) {
        return false;
      }
      else if (propValue instanceof String) {
        final String string = (String)propValue;
        return !"".equals(string) && !"false".equals(string) && !"0".equals(string); //NON-NLS
      }
      else if (propValue instanceof Boolean) {
        return (Boolean) propValue;
      }
      else if (propValue instanceof Integer) {
        return ((Integer) propValue) != 0;
      }
    }
    return true;
  }

  protected boolean checkProperties() {
    final boolean p1 = checkProperty(propertyName);
    if (propertyName.isEmpty()) return true;

    if (propertyName2.isEmpty()) {
      return p1;
    }
    else {
      final boolean p2 = checkProperty(propertyName2);
      final LogicalCompareMode mode = LogicalCompareMode.whichSymbol(compareMode);
      switch (mode) {
      case OR:
        return p1 || p2;
      case XOR:
        return (p1 || p2) && !(p1 && p2);
      case NOR:
        return !p1 && !p2;
      default:
        return p1 && p2;
      }
    }
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);

    if (checkProperties()) {
      border.draw(this, g, x, y, obs, zoom);
    }
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.BorderOutline.trait_description", propertyName + (((propertyName2 != null) && !propertyName2.isEmpty()) ? " " + compareMode + " " + propertyName2 : ""), description);
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.BorderOutline.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("BorderOutline.html"); // NON-NLS
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof BorderOutline)) return false;
    final BorderOutline c = (BorderOutline) o;
    if (! Objects.equals(color, c.color)) return false;
    if (! Objects.equals(propertyName, c.propertyName)) return false;
    if (! Objects.equals(propertyName2, c.propertyName2)) return false;
    if (! Objects.equals(compareMode, c.compareMode)) return false;
    return Objects.equals(thickness, c.thickness);
  }

  @Override
  public Object getProperty(Object key) {
    if (key.equals(Properties.VISIBLE_STATE)) {
      return String.valueOf(super.getProperty(key)) + checkProperties();
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (key.equals(Properties.VISIBLE_STATE)) {
      return getProperty(key);
    }
    return super.getLocalizedProperty(key);
  }


  private static class Ed implements PieceEditor {
    private final StringConfigurer propertyInput;
    private final LogicalCompareConfigurer compareInput;
    private final StringConfigurer propertyInput2;
    private final StringConfigurer descInput;
    private final IntConfigurer thicknessConfig;
    private final ColorConfigurer colorConfig;
    private final TraitConfigPanel box;

    private Ed(BorderOutline p) {

      box = new TraitConfigPanel();

      descInput = new StringConfigurer(p.description);
      descInput.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", descInput);

      propertyInput = new StringConfigurer(p.propertyName);
      box.add("Editor.BorderOutline.property_name", propertyInput);

      compareInput = new LogicalCompareConfigurer();
      compareInput.setValue(p.compareMode);
      box.add("Editor.BorderOutline.compare_mode", compareInput);

      propertyInput2 = new StringConfigurer(p.propertyName2);
      box.add("Editor.BorderOutline.property_name_2", propertyInput2);

      colorConfig = new ColorConfigurer(p.color);
      box.add("Editor.BorderOutline.color", colorConfig);

      thicknessConfig = new IntConfigurer(p.thickness);
      box.add("Editor.BorderOutline.thickness", thicknessConfig);
    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(propertyInput.getValueString()).append(descInput.getValueString()).append(thicknessConfig.getValueString()).append(colorConfig.getValueString()).append(propertyInput2.getValueString()).append(compareInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "false"; // NON-NLS
    }
  }

  /**
   * Happy little Configurer class for the Compare Modes
   */
  private static class LogicalCompareConfigurer extends TranslatingStringEnumConfigurer {
    LogicalCompareConfigurer() {
      super(null, null, LogicalCompareMode.getSymbols(), LogicalCompareMode.getKeys());
    }
  }

  /**
   * {@link SearchTarget}
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Arrays.asList(propertyName, propertyName2);
  }
}
