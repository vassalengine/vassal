/*
 * $Id$
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

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;

import javax.swing.BoxLayout;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;

import VASSAL.build.BadDataReport;
import VASSAL.tools.ColorButton;
import VASSAL.tools.ErrorDialog;

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

  public String getValueString() {
    return value == null ? "" : colorToString(colorValue());
  }

  public void setValue(Object o) {
    super.setValue(o);
    if (cb != null) {
      cb.setColor((Color) o);
    }
  }

  public void setValue(String s) {
    setValue(stringToColor(s));
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      p.add(new JLabel(getName()));

      cb = new ColorButton(colorValue());
      cb.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setValue(JColorChooser.showDialog(null, getName(), colorValue()));
        }
      });

      p.add(cb);
    }
    return p;
  }

  private Color colorValue() {
    return (Color) value;
  }

  public static String colorToString(Color c) {
    return c == null ? null :
        c.getRed() + ","
        + c.getGreen() + ","
        + c.getBlue();
  }

  public static Color stringToColor(String s) {
    if (s == null || s.length() == 0 || "null".equals(s)) {
      return null;
    }

    try {
      if (s.startsWith("0X") || s.startsWith("0x")) {
        return Color.decode(s);
      }
      else {
        final StringTokenizer st = new StringTokenizer(s, ",");
        return new Color(Integer.parseInt(st.nextToken()),
                         Integer.parseInt(st.nextToken()),
                         Integer.parseInt(st.nextToken()));
      }
    }
    catch (NumberFormatException e) {
      ErrorDialog.dataError(new BadDataReport("not an integer", s, e));
    }
    catch (IllegalArgumentException e) {
      ErrorDialog.dataError(new BadDataReport("bad color", s, e));
    }
    catch (NoSuchElementException e) {
      ErrorDialog.dataError(new BadDataReport("bad color", s, e));
    }

    // default to black in case of bad data
    return Color.BLACK;
  }
}
