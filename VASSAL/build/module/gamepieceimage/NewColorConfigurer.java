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
package VASSAL.build.module.gamepieceimage;

import java.awt.Color;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;

/**
 * Configurer for {@link Color} values
 */
public class NewColorConfigurer extends Configurer {
  private JPanel p;
  private Panel cp;

  public NewColorConfigurer(String key, String name) {
    this(key, name, Color.black);
  }

  public NewColorConfigurer(String key, String name, Color val) {
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

  protected Box colorBox;
  protected Box swatchBox;
  BooleanConfigurer bc;
  ColorSwatchConfigurer csc;
  
  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
      
      Box box = Box.createHorizontalBox();
      box.add(new JLabel("Use Named Colors?"));
      bc = new BooleanConfigurer(null, "", Boolean.FALSE);
      box.add(bc.getControls());
      bc.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          colorBox.setVisible(!bc.booleanValue().booleanValue());
          swatchBox.setVisible(bc.booleanValue().booleanValue());
          SwingUtilities.getWindowAncestor(bc.getControls()).pack();
        }   
      });
      p.add(box);
      
      colorBox = Box.createHorizontalBox();
      colorBox.add(new JLabel(getName()));
      cp = new Panel();
      cp.setMaximumSize(new java.awt.Dimension(40, 40));
      cp.setMinimumSize(new java.awt.Dimension(40, 40));
      cp.setSize(new java.awt.Dimension(40, 40));
      colorBox.add(cp);
      JButton b = new JButton("Select");
      colorBox.add(b);
      p.add(colorBox);
      
      b.addActionListener
          (new java.awt.event.ActionListener() {
            public void actionPerformed
                (java.awt.event.ActionEvent e) {
              setValue(JColorChooser.showDialog
                       (null, getName(), colorValue()));
              csc.setValue(new ColorSwatch("", (Color) getValue()));
            }
          });
      
      swatchBox = Box.createHorizontalBox();
      csc = new ColorSwatchConfigurer(null, "Select Color:", "WHITE");
      csc.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          setValue(csc.getValueColor());
        }   
      });
      swatchBox.add(csc.getControls());
      swatchBox.setVisible(false);
      p.add(swatchBox);
      
      
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
