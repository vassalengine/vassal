/*
 * $Id$
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
import java.awt.geom.AffineTransform;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.JComponent;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import VASSAL.configure.AbstractAttributeListConfigurable;
import VASSAL.configure.Attribute.*;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * Abstract base class for grid numbering classes for hexagonal and rectangular grids
 */
public abstract class RegularGridNumbering extends AbstractAttributeListConfigurable
implements GridNumbering {
  transient protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  protected FirstCoord first = FirstCoord.HORIZONTAL_FIRST;
  protected String sep = "";
  protected CoordType hType = CoordType.NUMERIC;
  protected CoordType vType = CoordType.NUMERIC;
  protected int hLeading = 1;
  protected int vLeading = 1;
  protected int hOff = 1;
  protected int vOff = 1;
  protected Boolean hDescend = false;
  protected Boolean vDescend = false;
  protected Boolean visible = false;
  protected int fontSize = 9;
  protected Color color = Color.black;
  protected AngleEnum rotateText = AngleEnum.___0;
  protected int hDrawOff = 0;
  protected int vDrawOff = 0;
  protected String locationFormat = "$" + GRID_LOCATION + "$";
  transient protected JComponent visualizer;
  transient protected FormattedString format = new FormattedString();

  transient public static final String GRID_LOCATION = "gridLocation";
  transient public static final String ROW = "row";
  transient public static final String COLUMN = "column";

  public enum FirstCoord { HORIZONTAL_FIRST, VERTICAL_FIRST; }
  public enum CoordType { ALPHABETIC, NUMERIC; }
  public enum AngleEnum {
    ___0, __30, __45, __60, __90, _120, _135, _150,
    _180, _210, _225, _240, _270, _300, _315, _330, _360;
    private AngleEnum() {
      valueDegrees = toInt(this.toString());
      valueRadians = Math.toRadians(valueDegrees);
    }
    public final int valueDegrees;
    public final double valueRadians;

    protected static Integer toInt(String string) {
      return Integer.valueOf(string.replace('_', ' ').trim());
    }
  }

  /**
   * The use of EnumCharAttribute and EnumIntAttribute is only required to support
   * the deprecated paradigm of serializing to a single Character value or an
   * unadorned integer, respectively, rather than an to enumeration identifier.
   * This practice is discouraged, as these classes may be removed in a
   * future release.
   * @throws NoSuchFieldException
   */
  @SuppressWarnings("deprecation")
  public RegularGridNumbering() {
    super(17);
    addAttribute(new EnumCharAttribute<FirstCoord>(FirstCoord.class, FIRST,  "Order:  ") {});
    addAttribute(SEP, "Separator:  ");
    addAttribute(new EnumCharAttribute<CoordType>(CoordType.class, H_TYPE,"Horizontal numbering:  ") {});
    addAttribute(H_LEADING, "Leading zeros in horizontal:  ",
        new VisibilityCondition() {
          @Override
          public boolean shouldBeVisible() { return hType == CoordType.NUMERIC; } });
    addAttribute(H_OFF, "Starting number in horizontal:  ");
    addAttribute(H_DESCEND, "Horizontal numbering descending?");
    addAttribute(new EnumCharAttribute<CoordType>(CoordType.class, V_TYPE,"Vertical numbering:  ") {});
    addAttribute(V_LEADING, "Leading zeros in vertical:  ",
        new VisibilityCondition() {
          @Override
          public boolean shouldBeVisible() { return vType == CoordType.NUMERIC; } });
    addAttribute(V_OFF, "Starting number in vertical:  ");
    addAttribute(V_DESCEND, "Vertical numbering descending?");
      addAttribute(new FormattedStringAttribute(LOCATION_FORMAT,"Location format:  ",
          new String[]{GRID_LOCATION, ROW, COLUMN}){
      });
    addAttribute(VISIBLE, "Draw Numbering?");
    addAttribute(FONT_SIZE, "Font size:  ",numberingVisiblity);
    addAttribute(new ColorAttribute(COLOR, "Color:  ",numberingVisiblity){});
    addAttribute(new EnumIntAttribute<AngleEnum>(AngleEnum.class, ROTATE_TEXT,
        "Rotate text (Degrees):  ") {
    });
    addAttribute(H_DRAW_OFF, "Text X offset:  ");
    addAttribute(V_DRAW_OFF, "Text Y offset:  ");
    }

  VisibilityCondition numberingVisiblity = new VisibilityCondition() {
      @Override
      public boolean shouldBeVisible() { return isVisible(); }
    };

  public abstract int getRow(Point p);
  public abstract int getColumn(Point p);
  @Override
  public boolean isVisible() { return visible; }
  @Override
  public void setVisible(boolean isVisible) { visible = isVisible; }

  @Override
  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  /**
   * Return a component that shows how the grid will draw itself
   */
  protected abstract JComponent getGridVisualizer();

  @Override
  public Configurer getConfigurer() {
    AutoConfigurer c = (AutoConfigurer) super.getConfigurer();
    String[] s = getAttributeNames();
    for (int i = 0; i < s.length; ++i) {
      c.getConfigurer(s[i]).addPropertyChangeListener(new PropertyChangeListener() {
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

  protected String getName(int row, int col) {
    String rowName = getNamePart(row + vOff, vType, vLeading);
    String colName = getNamePart(col + hOff, hType, hLeading);
    if (first == FirstCoord.HORIZONTAL_FIRST)
        return colName + sep + rowName;
    else
        return rowName + sep + colName;
  }

  // This appears to be the most efficient way to accomplish this without
  // using groups. It also helps when there is no separator between alphabetic
  // coordinates (as long as both coordinates don't use the same letter).
  // AAFF is not ambiguous, but AAAA is.  Coordinates like 04AB will fail.
  static final String ALPHABETIC_MATCH = "-?(?:A+|B+|C+|D+|E+|F+|G+|H+|I+|J+|K+|L+|M+|N+|O+|P+|Q+|R+|S+|T+|U+|V+|W+|X+|Y+|Z+)";

  protected String getMatchingPattern(CoordType type, int leading) {
    if (type == CoordType.ALPHABETIC)
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
          if (first == FirstCoord.HORIZONTAL_FIRST) {
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
    format.setProperty(ROW, getNamePart(row+vOff, vType, vLeading));
    format.setProperty(COLUMN, getNamePart(col+hOff, hType, hLeading));
    return format.getLocalizedText();
  }

  @Override
  public String localizedLocationName(Point pt) {
    return locationName(pt);
  }

  public static final String ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

  @SuppressWarnings("hiding")
  protected int parseName(String name, CoordType type) {
    int value = 0;
    if (type == CoordType.ALPHABETIC) {
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
    }
    else { // Numeric
      value = Integer.parseInt(name);
    }
    return value;
  }

  protected String getNamePart(int rowOrColumn, CoordType type, int leading) {
    String val = rowOrColumn < 0 ? "-" : "";
    rowOrColumn = Math.abs(rowOrColumn);
    if (type == CoordType.ALPHABETIC) {
        do {
          val += ALPHABET.charAt(rowOrColumn % 26);
          rowOrColumn -= 26;
        } while (rowOrColumn >= 0);
        return val;
    }
    else {
        while (leading > 0 && rowOrColumn < Math.pow(10.0, leading--)) {
          val += "0";
        }
        return val + rowOrColumn;
    }
  }

  /**
   * Translate the label center point based on the vector offset (p) and
   * the rotation factor. By tradition, use a left-handed coordinate system .
   */
  public Point offsetLabelCenter(Point p, double zoom) {
    return offsetLabelCenter(p.x, p.y, zoom);
  }

   /**
   * Translate the label center point based on the x, Y offset and
   * the rotation factor. By tradition, use a left-handed coordinate system.
   */
  public Point offsetLabelCenter(int x, int y, double zoom) {
    Point n = new Point(x, y);
    n = (Point) AffineTransform.getRotateInstance(-rotateText.valueRadians).transform(n, n);
    n.x += (hDrawOff * zoom);
    n.y += (vDrawOff * zoom);
    return n;
  }
}
