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
import java.awt.Font;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * Configurer for {@link Color} values
 */
public class ColorConfigurer extends Configurer {
  private static Font FONT = new Font("Dialog", 0, 10);
  private JPanel p;
  private Panel cp;

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
//	if (o == null)
//	    o = Color.black;
    super.setValue(o);
    if (cp != null)
      cp.repaint();
  }

  public void setValue(String s) {
    setValue(stringToColor(s));
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      p.add(new JLabel(getName()));
      cp = new Panel();
      cp.setMaximumSize(new java.awt.Dimension(40, 40));
      cp.setMinimumSize(new java.awt.Dimension(40, 40));
      cp.setSize(new java.awt.Dimension(40, 40));
      p.add(cp);
      JButton b = new JButton("Select");
      p.add(b);
      b.addActionListener
          (new java.awt.event.ActionListener() {
            public void actionPerformed
                (java.awt.event.ActionEvent e) {
              setValue(JColorChooser.showDialog
                       (null, getName(), colorValue()));
            }
          });
    }
    return p;
  }

  private Color colorValue() {
    return (Color) value;
  }

  private class Panel extends JPanel {
    public void paint(java.awt.Graphics g) {
      if (colorValue() != null) {
        g.setColor(colorValue());
        g.fillRect(0, 0, getSize().width, getSize().height);
      }
      else {
        g.clearRect(0, 0, getSize().width, getSize().height);
        g.setFont(FONT);
        g.drawString(" nil ", getSize().width / 2
                              - g.getFontMetrics(g.getFont()).stringWidth(" nil ") / 2, getSize().height / 2);
      }
    }
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
