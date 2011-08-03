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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;

import VASSAL.tools.ScrollPane;

/**
 * A Configurer for {@link Font} values
 */
public class FontConfigurer extends Configurer {
  private JPanel p;
  private JComboBox size;
  private JComboBox family;
  private int[] sizes;

  public FontConfigurer(String key, String name) {
    this(key, name, new Font("SansSerif", Font.PLAIN, 12));
  }

  public FontConfigurer(String key, String name, Font val) {
    this(key, name, val, new int[]{9, 10, 11, 12, 15, 18});
  }

  public FontConfigurer(String key, String name, Font val, int[] sizes) {
    super(key, name, val);
    this.sizes = sizes;
  }

  public String getValueString() {
    return encode((Font) value);
  }

  public void setValue(String s) {
    setValue(decode(s));
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      p.add(new JLabel(name));
      family = new JComboBox();
      String[] s = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
      for (int i = 0; i < s.length; ++i) {
        family.addItem(s[i]);
      }
      family.setSelectedItem(value == null ? "SansSerif" : ((Font) value).getFamily());
      family.setMaximumSize(new Dimension(family.getMaximumSize().width,family.getPreferredSize().height));
      p.add(family);

      size = new JComboBox();
      for (int i = 0; i < sizes.length; ++i) {
        size.addItem(sizes[i] + "");
      }
      size.setSelectedItem(value == null ? sizes[sizes.length / 2] + ""
                           : ((Font) value).getSize() + "");
      size.setMaximumSize(new Dimension(size.getMaximumSize().width,size.getPreferredSize().height));
      p.add(size);

      ItemListener l = new ItemListener() {
        public void itemStateChanged(ItemEvent evt) {
          setValue(new Font((String) family.getSelectedItem(),
                            Font.PLAIN,
                            Integer.parseInt((String) size.getSelectedItem())));
        }
      };
      size.addItemListener(l);
      family.addItemListener(l);
    }
    return p;
  }

  public static Font decode(String s) {
    int i = s.indexOf(',');
    return new Font(s.substring(0, i), Font.PLAIN, Integer.parseInt(s.substring(i + 1)));
  }

  public static String encode(Font f) {
    return f.getName() + "," + f.getSize();
  }

  public static void main(String args[]) {
    final JFrame f = new JFrame();
    f.setLayout(new BoxLayout(f.getContentPane(), BoxLayout.Y_AXIS));
    FontConfigurer c = new FontConfigurer("a", "Font: ", null, new int[]{4, 5, 6, 13});
    f.add(c.getControls());
    final JTextArea tf = new JTextArea();
    tf.setText("The quick brown fox jumps over the lazy dog.");
    f.add(new ScrollPane(tf));
    c.addPropertyChangeListener(new java.beans.PropertyChangeListener() {
      public void propertyChange(java.beans.PropertyChangeEvent evt) {
        Font font = (Font) evt.getNewValue();
        FontConfigurer fc = new FontConfigurer(null, null, font);
        fc.setValue(fc.getValueString());
        font = (Font) fc.getValue();
        tf.setFont(font);
        f.pack();
      }
    });
    f.pack();
    f.setVisible(true);
  }
}

