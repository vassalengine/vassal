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
package VASSAL.configure;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;

import VASSAL.tools.ColorButton;

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
    if (cb != null) 
      cb.setColor((Color) o);
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
    if (s == null
        || "null".equals(s)) {
      return null;
    }
    java.util.StringTokenizer st = new java.util.StringTokenizer(s, ",");
    try {
      return new Color(Integer.parseInt(st.nextToken()),
                       Integer.parseInt(st.nextToken()),
                       Integer.parseInt(st.nextToken()));
    }
    catch (Exception e) {
      return null;
    }
  }
}
