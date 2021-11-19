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
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.StringEnum;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

import java.awt.Color;
import java.awt.Container;
import java.awt.Point;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JComponent;

/**
 * Abstract base class for grid numbering classes for hexagonal and rectangular grids
 */
public abstract class RegularGridNumbering extends AbstractConfigurable implements GridNumbering {
  protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  protected char first = 'H';
  protected String sep = "";
  protected char hType = 'N';
  protected char vType = 'N';
  protected int hLeading = 1;
  protected int vLeading = 1;
  protected int hOff = 1;
  protected int vOff = 1;
  protected boolean hDescending = false;
  protected boolean vDescending = false;
  protected boolean visible = false;
  protected int fontSize = 9;
  protected Color color = Color.black;
  protected int rotateTextDegrees = 0;
  protected int hDrawOff = 0;
  protected int vDrawOff = 0;
  protected JComponent visualizer;
  protected String locationFormat = "$" + GRID_LOCATION + "$";
  protected FormattedString format = new FormattedString();

  public static final String FIRST = "first"; //NON-NLS
  public static final String SEP = "sep"; //NON-NLS
  public static final String H_TYPE = "hType"; //NON-NLS
  public static final String V_TYPE = "vType"; //NON-NLS
  public static final String H_LEADING = "hLeading"; //NON-NLS
  public static final String V_LEADING = "vLeading"; //NON-NLS
  public static final String H_OFF = "hOff"; //NON-NLS
  public static final String V_OFF = "vOff"; //NON-NLS
  public static final String V_DESCEND = "vDescend"; //NON-NLS
  public static final String H_DESCEND = "hDescend"; //NON-NLS
  public static final String FONT_SIZE = "fontSize"; //NON-NLS
  public static final String COLOR = "color"; //NON-NLS
  public static final String VISIBLE = "visible"; //NON-NLS
  public static final String ROTATE_TEXT = "rotateText"; //NON-NLS
  public static final String H_DRAW_OFF = "hDrawOff"; //NON-NLS
  public static final String V_DRAW_OFF = "vDrawOff"; //NON-NLS
  public static final String LOCATION_FORMAT = "locationFormat"; //NON-NLS
  public static final String GRID_LOCATION = "gridLocation"; //NON-NLS
  public static final String ROW = "row"; //NON-NLS
  public static final String COLUMN = "column"; //NON-NLS

  public static final double VISUALIZER_GRID_SIZE = 100.0;

  @Override
  public String getAttributeValueString(String key) {
    if (FIRST.equals(key)) {
      return String.valueOf(first);
    }
    else if (SEP.equals(key)) {
      return sep;
    }
    else if (H_TYPE.equals(key)) {
      return String.valueOf(hType);
    }
    else if (V_TYPE.equals(key)) {
      return String.valueOf(vType);
    }
    else if (H_LEADING.equals(key)) {
      return String.valueOf(hLeading);
    }
    else if (V_LEADING.equals(key)) {
      return String.valueOf(vLeading);
    }
    else if (H_OFF.equals(key)) {
      return String.valueOf(hOff);
    }
    else if (V_OFF.equals(key)) {
      return String.valueOf(vOff);
    }
    else if (H_DESCEND.equals(key)) {
      return String.valueOf(hDescending);
    }
    else if (V_DESCEND.equals(key)) {
      return String.valueOf(vDescending);
    }
    else if (FONT_SIZE.equals(key)) {
      return String.valueOf(fontSize);
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else if (VISIBLE.equals(key)) {
      return String.valueOf(visible);
    }
    else if (LOCATION_FORMAT.equals(key)) {
      return locationFormat;
    }
    else if (ROTATE_TEXT.equals(key)) {
      return String.valueOf(rotateTextDegrees);
    }
    else if (H_DRAW_OFF.equals(key)) {
      return String.valueOf(hDrawOff);
    }
    else if (V_DRAW_OFF.equals(key)) {
      return String.valueOf(vDrawOff);
    }
    else {
      return null;
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (FIRST.equals(key)) {
      first = ((String) value).charAt(0);
    }
    else if (SEP.equals(key)) {
      sep = (String) value;
    }
    else if (H_TYPE.equals(key)) {
      hType = ((String) value).charAt(0);
    }
    else if (V_TYPE.equals(key)) {
      vType = ((String) value).charAt(0);
    }
    else if (H_LEADING.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      hLeading = (Integer) value;
    }
    else if (V_LEADING.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      vLeading = (Integer) value;
    }
    else if (H_OFF.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      hOff = (Integer) value;
    }
    else if (V_OFF.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      vOff = (Integer) value;
    }
    else if (H_DESCEND.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      hDescending = (Boolean) value;
    }
    else if (V_DESCEND.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      vDescending = (Boolean) value;
    }
    else if (FONT_SIZE.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      fontSize = (Integer) value;
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      color = (Color) value;
    }
    else if (VISIBLE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      visible = (Boolean) value;
    }
    else if (LOCATION_FORMAT.equals(key)) {
      locationFormat = (String) value;
    }
    else if (ROTATE_TEXT.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      rotateTextDegrees = (Integer) value;
    }
    else if (H_DRAW_OFF.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      hDrawOff = (Integer) value;
    }
    else if (V_DRAW_OFF.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      vDrawOff = (Integer) value;
    }
  }

  @Override
  public boolean isVisible() {
    return visible;
  }

  public abstract int getRow(Point p);

  public abstract int getColumn(Point p);

  @Override
  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      FIRST,
      SEP,
      H_TYPE,
      H_LEADING,
      H_OFF,
      H_DESCEND,
      V_TYPE,
      V_LEADING,
      V_OFF,
      V_DESCEND,
      LOCATION_FORMAT,
      VISIBLE,
      FONT_SIZE,
      COLOR,
      ROTATE_TEXT,
      H_DRAW_OFF,
      V_DRAW_OFF
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.RegularGridNumbering.order"),
      Resources.getString("Editor.RegularGridNumbering.separator"),
      Resources.getString("Editor.RegularGridNumbering.horizontal_numbering"),
      Resources.getString("Editor.RegularGridNumbering.leading_zeros_in_horizontal"),
      Resources.getString("Editor.RegularGridNumbering.starting_number_in_horizontal"),
      Resources.getString("Editor.RegularGridNumbering.horizontal_numbering_descending"),
      Resources.getString("Editor.RegularGridNumbering.vertical_numbering"),
      Resources.getString("Editor.RegularGridNumbering.leading_zeros_in_vertical"),
      Resources.getString("Editor.RegularGridNumbering.starting_number_in_vertical"),
      Resources.getString("Editor.RegularGridNumbering.vertical_numbering_descending"),
      Resources.getString("Editor.RegularGridNumbering.location_format"),
      Resources.getString("Editor.RegularGridNumbering.draw_numbering"),
      Resources.getString("Editor.RegularGridNumbering.font_size"),
      Resources.getString("Editor.color_label"),
      Resources.getString("Editor.RegularGridNumbering.rotate_text_degrees"),
      Resources.getString("Editor.RegularGridNumbering.text_x_offset"),
      Resources.getString("Editor.RegularGridNumbering.text_y_offset")
    };
  }

  public static class F extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{"Horizontal first", "Vertical first"}; //NON-NLS (really)
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.RegularGridNumbering.horizontal_first",
        "Editor.RegularGridNumbering.vertical_first",
      };
    }
  }

  public static class T extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{"Numerical", "Alphabetic"}; //NON-NLS
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.RegularGridNumbering.numerical",
        "Editor.RegularGridNumbering.alphabetic",
      };
    }
  }

  public static class LocationFormatConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{GRID_LOCATION, ROW, COLUMN});
    }
  }

  public static class R extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{"0", "90", "180", "270"};
    }
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      F.class,
      String.class,
      T.class,
      Integer.class,
      Integer.class,
      Boolean.class,
      T.class,
      Integer.class,
      Integer.class,
      Boolean.class,
      LocationFormatConfig.class,
      Boolean.class,
      Integer.class,
      Color.class,
      R.class,
      Integer.class,
      Integer.class
    };
  }

  /**
   * Return a component that shows how the grid will draw itself
   */
  protected abstract JComponent getGridVisualizer();

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (FONT_SIZE.equals(name)
        || COLOR.equals(name)) {
      return () -> visible;
    }
    else if (H_LEADING.equals(name)) {
      return () -> hType == 'N';
    }
    else if (V_LEADING.equals(name)) {
      return () -> vType == 'N';
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  @Override
  public Configurer getConfigurer() {
    final AutoConfigurer c = (AutoConfigurer) super.getConfigurer();
    final String[] s = getAttributeNames();
    for (final String value : s) {
      c.getConfigurer(value).addPropertyChangeListener(evt -> visualizer.repaint());
    }
    final JComponent gridVisualizer = getGridVisualizer();
    if (gridVisualizer != null) {
      ((Container) c.getControls()).add(gridVisualizer, "span 2, align left"); //NON-NLS
    }
    return c;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.RegularGridNumbering.component_type");
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GridNumbering.html"); //NON-NLS
  }

  protected String getName(int row, int column) {
    final String rowName = getName(row + vOff, vType, vLeading);
    final String colName = getName(column + hOff, hType, hLeading);
    if (first == 'H') {
      return colName + sep + rowName;
    }
    return rowName + sep + colName;
  }

  // This appears to be the most efficient way to accomplish this without
  // using groups. It also helps when there is no separator between alphabetic
  // coordinates (as long as both coordinates don't use the same letter).
  // AAFF is not ambiguous, but AAAA is.  Coordinates like 04AB will fail.
  static final String ALPHABETIC_MATCH = "-?(?:A+|B+|C+|D+|E+|F+|G+|H+|I+|J+|K+|L+|M+|N+|O+|P+|Q+|R+|S+|T+|U+|V+|W+|X+|Y+|Z+)"; //NON-NLS

  protected String getMatchingPattern(char type, int leading) {
    if (type == 'A')
      return ALPHABETIC_MATCH;
    else
      return "-?[0-9]{" + (leading + 1) + ",}";
  }

  @Override
  public Point getLocation(String location) throws BadCoords {

    final SequenceEncoder.Decoder se = new SequenceEncoder.Decoder(locationFormat, '$');
    boolean isProperty = true;
    final StringBuilder regex = new StringBuilder();
    int colGroup = 0;
    int rowGroup = 0;
    int groupCount = 0;
    while (se.hasMoreTokens()) {
      final String token = se.nextToken();
      isProperty = !isProperty;
      if (token.length() > 0) {
        if (!isProperty || !se.hasMoreTokens()) {
          regex.append(Pattern.quote(token));
        }
        else if (token.equals(GRID_LOCATION)) {
          if (first == 'H') {
            regex.append('(').append(getMatchingPattern(hType, hLeading)).append(')');
            colGroup = ++groupCount;
            if (sep.length() > 0)
              regex.append(Pattern.quote(sep));
            regex.append('(').append(getMatchingPattern(vType, vLeading)).append(')');
            rowGroup = ++groupCount;
          }
          else {
            regex.append('(').append(getMatchingPattern(vType, vLeading)).append(')');
            rowGroup = ++groupCount;
            regex.append(Pattern.quote(sep));
            regex.append('(').append(getMatchingPattern(hType, hLeading)).append(')');
            colGroup = ++groupCount;
          }
        }
        else if (token.equals(ROW)) {
          regex.append('(').append(getMatchingPattern(vType, vLeading)).append(')');
          rowGroup = ++groupCount;
        }
        else if (token.equals(COLUMN)) {
          regex.append('(').append(getMatchingPattern(hType, hLeading)).append(')');
          colGroup = ++groupCount;
        }
      }
    }

    if (regex.length() == 0 || colGroup == 0 || rowGroup == 0)
      throw new BadCoords();

    final Pattern pattern = Pattern.compile(regex.toString());
    final Matcher matcher = pattern.matcher(location);
    if (!matcher.matches()) {
// FIXME: rename to BadCoordsException
      throw new BadCoords();
    }
    assert (matcher.groupCount() == groupCount && groupCount >= 2);

    final String rowName = location.substring(matcher.start(rowGroup), matcher.end(rowGroup));
    final String colName = location.substring(matcher.start(colGroup), matcher.end(colGroup));
    final int row = parseName(rowName, vType);
    final int col = parseName(colName, hType);

    return getCenterPoint(col - hOff, row - vOff);
  }

  public abstract Point getCenterPoint(int col, int row);

  @Override
  public String locationName(Point pt) {
    final int row = getRow(pt);
    final int col = getColumn(pt);
    format.setFormat(locationFormat);
    format.setProperty(GRID_LOCATION, getName(row, col));
    format.setProperty(ROW, getName(row + vOff, vType, vLeading));
    format.setProperty(COLUMN, getName(col + hOff, hType, hLeading));
    return format.getLocalizedText(this, "Editor.RegularGridNumbering.location_format");
  }

  @Override
  public String localizedLocationName(Point pt) {
    return locationName(pt);
  }

  public static final String ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; //NON-NLS

  protected int parseName(String name, char type) {
    int value = 0;

    if (type == 'A') { // Alphabetic
      int index = 0;
      boolean negative = false;
      if (name.startsWith("-")) {
        negative = true;
        ++index;
      }
      while (index < name.length() && Character.isUpperCase(name.charAt(index))) {
        if (index < name.length() - 1)
          value += 26;
        else
          value += ALPHABET.indexOf(name.charAt(index));
        ++index;
      }
      if (negative)
        value *= -1;
    }
    else { // Numeric
      value = Integer.parseInt(name);
    }

    return value;
  }

  protected String getName(int rowOrColumn, char type, int leading) {
    String val = rowOrColumn < 0 ? "-" : "";
    rowOrColumn = Math.abs(rowOrColumn);

    if (type == 'A') { // Alphabetic
      do {
        val += ALPHABET.charAt(rowOrColumn % 26);
        rowOrColumn -= 26;
      } while (rowOrColumn >= 0);
      return val;
    }
    while (leading > 0 && rowOrColumn < Math.pow(10.0, leading--)) {
      val += "0"; // Numeric
    }
    return val + rowOrColumn;
  }

  /*
   * Translate the label center point based on the x, Y offset and
   * the rotation factor
   */
  public Point offsetLabelCenter(Point p, double zoom) {
    return offsetLabelCenter(p.x, p.y, zoom);
  }

  public Point offsetLabelCenter(int x, int y, double zoom) {
    final Point n = new Point(x, y);
    switch (rotateTextDegrees) {
    case 0:
      break;
    case 90:
      n.x = y;
      n.y = -x;
      break;
    case 180:
      n.x = -x;
      n.y = -y;
      break;
    case 270:
      n.x = -y;
      n.y = x;
      break;
    default  :
      break;
    }
    n.x += (hDrawOff * zoom);
    n.y += (vDrawOff * zoom);

    return n;
  }
}
