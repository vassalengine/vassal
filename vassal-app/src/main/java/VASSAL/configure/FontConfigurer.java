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
package VASSAL.configure;

import VASSAL.tools.ScrollPane;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.awt.event.ItemListener;

import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextArea;

/**
 * A Configurer for {@link Font} values
 */
public class FontConfigurer extends Configurer {
  private JPanel p;
  private JComboBox<Integer> size;
  private JComboBox<String> family;
  private final int[] sizes;

  public FontConfigurer(String key, String name) {
    this(key, name, new Font(Font.SANS_SERIF, Font.PLAIN, 12));
  }

  public FontConfigurer(String key, String name, Font val) {
    this(key, name, val, new int[]{9, 10, 11, 12, 15, 18});
  }

  public FontConfigurer(String key, String name, Font val, int[] sizes) {
    super(key, name, val);
    this.sizes = sizes;
  }

  @Override
  public String getValueString() {
    return encode((Font) value);
  }

  @Override
  public void setValue(String s) {
    setValue(decode(s));
  }

  @Override
  public java.awt.Component getControls() {
    if (p == null) {
      p = new ConfigurerPanel(getName(), "[]rel[]", "[]rel[]rel[]"); // NON-NLS
      family = new JComboBox<>();
      final String[] s = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
      for (final String element : s) {
        family.addItem(element);
      }
      family.setSelectedItem(value == null ? Font.SANS_SERIF : ((Font) value).getFamily()); //NON-NLS
      family.setMaximumSize(new Dimension(family.getMaximumSize().width, family.getPreferredSize().height));
      p.add(family);

      size = new JComboBox<>();
      for (final int item : sizes) {
        size.addItem(item);
      }
      size.setSelectedItem(
        value == null ? sizes[sizes.length / 2] : ((Font) value).getSize()
      );
      size.setMaximumSize(new Dimension(size.getMaximumSize().width, size.getPreferredSize().height));
      p.add(size);

      final ItemListener l = evt -> setValue(new Font(
        (String) family.getSelectedItem(),
        Font.PLAIN,
        (Integer) size.getSelectedItem()
      ));
      size.addItemListener(l);
      family.addItemListener(l);
    }
    return p;
  }

  public static Font decode(String s) {
    final int i = s.indexOf(',');
    return new Font(s.substring(0, i), Font.PLAIN, Integer.parseInt(s.substring(i + 1)));
  }

  public static String encode(Font f) {
    return f.getName() + "," + f.getSize();
  }

  @Override
  public void setLabelVisible(boolean visible) {
    if (p instanceof ConfigurerPanel) {
      ((ConfigurerPanel) p).setLabelVisibility(visible);
    }
  }

  public static void main(String[] args) {
    final JFrame f = new JFrame();
    f.setLayout(new BoxLayout(f.getContentPane(), BoxLayout.Y_AXIS));
    final FontConfigurer c = new FontConfigurer("a", "Font: ", null, new int[]{4, 5, 6, 13}); //NON-NLS
    f.add(c.getControls());
    final JTextArea tf = new JTextArea();
    tf.setText("The quick brown fox jumps over the lazy dog."); //NON-NLS
    f.add(new ScrollPane(tf));
    c.addPropertyChangeListener(evt -> {
      Font font = (Font) evt.getNewValue();
      final FontConfigurer fc = new FontConfigurer(null, null, font);
      fc.setValue(fc.getValueString());
      font = (Font) fc.getValue();
      tf.setFont(font);
      f.pack();
    });
    f.pack();
    f.setVisible(true);
  }
}
