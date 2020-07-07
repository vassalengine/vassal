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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 21, 2002
 * Time: 10:18:26 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import java.awt.Color;
import java.awt.Container;
import java.awt.Point;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JComponent;

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
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

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

  public static final String FIRST = "first";
  public static final String SEP = "sep";
  public static final String H_TYPE = "hType";
  public static final String V_TYPE = "vType";
  public static final String H_LEADING = "hLeading";
  public static final String V_LEADING = "vLeading";
  public static final String H_OFF = "hOff";
  public static final String V_OFF = "vOff";
  public static final String V_DESCEND = "vDescend";
  public static final String H_DESCEND = "hDescend";
  public static final String FONT_SIZE = "fontSize";
  public static final String COLOR = "color";
  public static final String VISIBLE = "visible";
  public static final String ROTATE_TEXT = "rotateText";
  public static final String H_DRAW_OFF = "hDrawOff";
  public static final String V_DRAW_OFF = "vDrawOff";
  public static final String LOCATION_FORMAT = "locationFormat";
  public static final String GRID_LOCATION = "gridLocation";
  public static final String ROW = "row";
  public static final String COLUMN = "column";

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
    return new String[]{FIRST, SEP, H_TYPE, H_LEADING, H_OFF, H_DESCEND,
                        V_TYPE, V_LEADING, V_OFF, V_DESCEND, LOCATION_FORMAT, VISIBLE, FONT_SIZE, COLOR,
                        ROTATE_TEXT, H_DRAW_OFF, V_DRAW_OFF};
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{"Order:  ",
                        "Separator:  ",
                        "Horizontal numbering:  ",
                        "Leading zeros in horizontal:  ",
                        "Starting number in horizontal:  ",
                        "Horizontal numbering descending?",
                        "Vertical numbering:  ",
                        "Leading zeros in vertical:  ",
                        "Starting number in vertical:  ",
                        "Vertical numbering descending?",
                        "Location format:  ",
                        "Draw Numbering?",
                        "Font size:  ",
                        "Color:  ",
                        "Rotate text (Degrees):  ",
                        "Text X offset:  ",
                        "Text Y offset:  "};
  }

  public static class F extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{"Horizontal first", "Vertical first"};
    }
  }

  public static class T extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{"Numerical", "Alphabetic"};
    }
  }

  public static class LocationFormatConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{GRID_LOCATION,ROW,COLUMN});
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
      VisibilityCondition cond = new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return visible;
        }
      };
      return cond;
    }
    else if (H_LEADING.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return hType == 'N';
        }
      };
    }
    else if (V_LEADING.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return vType == 'N';
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  @Override
  public Configurer getConfigurer() {
    AutoConfigurer c = (AutoConfigurer) super.getConfigurer();
    String[] s = getAttributeNames();
    for (String value : s) {
      c.getConfigurer(value).addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          visualizer.repaint();
        }
      });
    }
    ((Container) c.getControls()).add(getGridVisualizer());
    return c;
  }

  public static String getConfigureTypeName() {
    return "Grid Numbering";
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GridNumbering.htm");
  }

  protected String getName(int row, int column) {
    String rowName = getName(row + vOff, vType, vLeading);
    String colName = getName(column + hOff, hType, hLeading);
    switch (first) {
    case 'H':
      return colName + sep + rowName;
    default:
      return rowName + sep + colName;
    }
  }

  // This appears to be the most efficient way to accomplish this without
  // using groups. It also helps when there is no separator between alphabetic
  // coordinates (as long as both coordinates don't use the same letter).
  // AAFF is not ambiguous, but AAAA is.  Coordinates like 04AB will fail.
  static final String ALPHABETIC_MATCH = "-?(?:A+|B+|C+|D+|E+|F+|G+|H+|I+|J+|K+|L+|M+|N+|O+|P+|Q+|R+|S+|T+|U+|V+|W+|X+|Y+|Z+)";

  protected String getMatchingPattern(char type, int leading) {
    if (type == 'A')
      return ALPHABETIC_MATCH;
    else
      return "-?[0-9]{" + (leading+1) + ",}";
  }

  @Override
  public Point getLocation(String location) throws BadCoords {

    SequenceEncoder.Decoder se = new SequenceEncoder.Decoder(locationFormat, '$');
    boolean isProperty = true;
    final StringBuilder regex = new StringBuilder();
    int colGroup = 0;
    int rowGroup = 0;
    int groupCount = 0;
    while (se.hasMoreTokens()) {
      String token = se.nextToken();
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

    Pattern pattern = Pattern.compile(regex.toString());
    Matcher matcher = pattern.matcher(location);
    if (!matcher.matches()) {
// FIXME: rename to BadCoordsException
      throw new BadCoords();
    }
    assert(matcher.groupCount() == groupCount && groupCount >= 2);

    String rowName = location.substring(matcher.start(rowGroup), matcher.end(rowGroup));
    String colName = location.substring(matcher.start(colGroup), matcher.end(colGroup));
    int row = parseName(rowName, vType);
    int col = parseName(colName, hType);

    return getCenterPoint(col-hOff, row-vOff);
  }

  public abstract Point getCenterPoint(int col, int row);

  @Override
  public String locationName(Point pt) {
    int row = getRow(pt);
    int col = getColumn(pt);
    format.setFormat(locationFormat);
    format.setProperty(GRID_LOCATION, getName(row, col));
    format.setProperty(ROW, getName(row+vOff, vType, vLeading));
    format.setProperty(COLUMN, getName(col+hOff, hType, hLeading));
    return format.getLocalizedText();
  }

  @Override
  public String localizedLocationName(Point pt) {
    return locationName(pt);
  }

  public static final String ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  protected int parseName(String name, char type) {
    int value = 0;
    switch (type) {
    case 'A': // Alphabetic
      int index = 0;
      boolean negative = false;
      if (name.startsWith("-")) {
        negative = true;
        ++index;
      }
      while (index < name.length() && Character.isUpperCase(name.charAt(index))) {
        if (index < name.length()-1)
          value += 26;
        else
          value += ALPHABET.indexOf(name.charAt(index));
        ++index;
      }
      if (negative)
        value *= -1;
      break;
    default: // Numeric
      value = Integer.parseInt(name);
    }

    return value;
  }

  protected String getName(int rowOrColumn, char type, int leading) {
    String val = rowOrColumn < 0 ? "-" : "";
    rowOrColumn = Math.abs(rowOrColumn);
    switch (type) {
    case 'A': // Alphabetic
      do {
        val += ALPHABET.charAt(rowOrColumn % 26);
        rowOrColumn -= 26;
      } while (rowOrColumn >= 0);
      return val;
    default: // Numeric
      while (leading > 0 && rowOrColumn < Math.pow(10.0, leading--)) {
        val += "0";
      }
      return val + rowOrColumn;
    }
  }

  /*
   * Translate the label center point based on the x, Y offset and
   * the rotation factor
   */
  public Point offsetLabelCenter(Point p, double zoom) {
    return offsetLabelCenter(p.x, p.y, zoom);
  }

  public Point offsetLabelCenter(int x, int y, double zoom) {
    Point n = new Point(x, y);
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
