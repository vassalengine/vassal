/*
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Brent Easton, Joel Uckelman
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
package VASSAL.configure;

import VASSAL.build.BadDataReport;
import VASSAL.tools.ColorButton;
import VASSAL.tools.ErrorDialog;

import javax.swing.JColorChooser;
import javax.swing.JPanel;
import java.awt.Color;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;

/**
 * Configurer for {@link Color} values.
 */
public class ColorConfigurer extends Configurer {
  private JPanel p;
  private ColorButton cb;

  public ColorConfigurer(String key, String name) {
    this(key, name, Color.black);
  }

  public ColorConfigurer(String key, String name, Color val) {
    super(key, name, val);
  }

  public ColorConfigurer(Color val) {
    super(null, null, val);
  }

  @Override
  public String getValueString() {
    return value == null ? "" : colorToString(colorValue());
  }

  @Override
  public void setValue(Object o) {
    super.setValue(o);
    if (cb != null) {
      cb.setColor((Color) o);
    }
  }

  @Override
  public void setValue(String s) {
    setValue(stringToColor(s));
  }

  @Override
  public java.awt.Component getControls() {
    if (p == null) {
      p = new ConfigurerPanel(getName(), "[]", "[][]"); // NON-NLS

      cb = new ColorButton(colorValue());
      cb.addActionListener(e -> setValue(JColorChooser.showDialog(null, getName(), colorValue())));

      p.add(cb, "grow 0"); // NON-NLS Make sure the Color button doesn't stretch.
    }
    return p;
  }

  private Color colorValue() {
    return (Color) value;
  }

  public static String colorToString(Color c) {
    if (c == null) {
      return ""; 
    }
    else if (c.getTransparency() == c.OPAQUE) {
      return c.getRed() + ","
        + c.getGreen() + ","
        + c.getBlue();
    }
    else {
      return c.getRed() + ","
        + c.getGreen() + ","
        + c.getBlue() + ","
        + c.getAlpha();
    }
  }

  public static Color stringToColor(String s) {
    if (s == null || s.length() == 0 || "null".equals(s)) { //NON-NLS
      return null;
    }

    try {
      if (s.startsWith("0X") || s.startsWith("0x")) {  //NON-NLS
        return Color.decode(s);
      }
      else {
        final StringTokenizer st = new StringTokenizer(s, ",");
        if (st.countTokens() > 3) { // has alpha value
          return new Color(Integer.parseInt(st.nextToken()),
                           Integer.parseInt(st.nextToken()),
                           Integer.parseInt(st.nextToken()),
                           Integer.parseInt(st.nextToken()));
        }
        else { // no alpha
          return new Color(Integer.parseInt(st.nextToken()),
                           Integer.parseInt(st.nextToken()),
                           Integer.parseInt(st.nextToken()));
        }
      }
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataWarning(new BadDataReport("not an integer", s, e)); //NON-NLS
    }
    catch (IllegalArgumentException | NoSuchElementException e) {
      ErrorDialog.dataWarning(new BadDataReport("bad color", s, e)); //NON-NLS
    }

    // default to black in case of bad data
    return Color.BLACK;
  }

  @Override
  public void setLabelVisible(boolean visible) {
    if (p instanceof ConfigurerPanel) {
      ((ConfigurerPanel) p).setLabelVisibility(visible);
    }
  }
}
