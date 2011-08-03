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

import java.awt.Font;
import java.awt.Window;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.tools.SequenceEncoder;

/**
 * A Configurer for {@link Font}values
 */
public class FontConfigurer extends Configurer {

  protected JPanel p;
  protected IntConfigurer size;
  protected BooleanConfigurer bold;
  protected BooleanConfigurer italic;
  protected BooleanConfigurer outline;
  protected JComboBox family;
  protected JTextField demo;

  public FontConfigurer(String key, String name) {
    super(key, name);
  }

  public FontConfigurer(String key, String name, OutlineFont f) {
    super(key, name);
    setValue(f);
  }

  public FontConfigurer(String key, String name, FontStyle f) {
    super(key, name);
    setValue(f.font);
    setName(f.getConfigureName());
  }
  public String getValueString() {
    return encode((OutlineFont) value);
  }

  public void setValue(String s) {
    setValue(decode(s));
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();

      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      box.add(new JLabel("Font Family:  "));

      family = new JComboBox();
      //String[] s = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
      for (int i = 0; i < FontManager.ALLOWABLE_FONTS.length; ++i) {
        family.addItem(FontManager.ALLOWABLE_FONTS[i]);
      }
      family.setSelectedItem(value == null ? FontManager.SANS_SERIF : (getFontValue().getFamily()));
      box.add(family);
      p.add(box);

      size = new IntConfigurer(null, "Size:  ", getFontValue().getSize());
      p.add(size.getControls());

      box = Box.createHorizontalBox();
      bold = new BooleanConfigurer(null, "Bold", isBold());
      box.add(bold.getControls());
      italic = new BooleanConfigurer(null, "Italic", isItalic());
      box.add(italic.getControls());
      outline = new BooleanConfigurer(null, "Outline", isOutline());
      box.add(outline.getControls());
      p.add(box);

      box = Box.createHorizontalBox();
      box.add(new JLabel("Sample:  "));
      demo = new JTextField("The quick brown fox", 20);
      demo.setEditable(false);
      box.add(demo);
      p.add(box);

      updateValue();

      ItemListener l = new ItemListener() {
        public void itemStateChanged(ItemEvent evt) {
          updateValue();
        }
      };
      family.addItemListener(l);

      PropertyChangeListener pc = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          updateValue();
        }
      };
      size.addPropertyChangeListener(pc);
      bold.addPropertyChangeListener(pc);
      italic.addPropertyChangeListener(pc);
      outline.addPropertyChangeListener(pc);
    }
    return p;
  }

  protected void updateValue() {
    final int style = Font.PLAIN |
       (bold.booleanValue().booleanValue() ? Font.BOLD : 0) |
       (italic.booleanValue().booleanValue() ? Font.ITALIC : 0);

    final OutlineFont font = new OutlineFont(
      (String) family.getSelectedItem(),
      style,
      Integer.parseInt(size.getValueString()),
      outline.booleanValue().booleanValue()
    );

    setValue(font);
    demo.setFont(font);

    final Window w = SwingUtilities.getWindowAncestor(getControls());
    if (w != null) {
      w.pack();
    }
  }

  protected OutlineFont getFontValue() {
    return (OutlineFont) getValue();
  }

  public static OutlineFont decode(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    return new OutlineFont(
        sd.nextToken(FontManager.DIALOG),
        sd.nextInt(Font.PLAIN),
        sd.nextInt(10),
        sd.nextBoolean(false));
  }

  public static String encode(OutlineFont f) {
    SequenceEncoder se = new SequenceEncoder(f.getName(), ',');
    se.append(f.getStyle());
    se.append(f.getSize());
    se.append(f.isOutline());
    return se.getValue();
  }

  public boolean isBold() {
    return (getFontValue().getStyle() & Font.BOLD) != 0;
  }

  public boolean isItalic() {
    return (getFontValue().getStyle() & Font.ITALIC) != 0;
  }

  public boolean isOutline() {
    return getFontValue().isOutline();
  }
}
