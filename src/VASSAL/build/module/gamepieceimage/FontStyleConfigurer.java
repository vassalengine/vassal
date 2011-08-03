/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package VASSAL.build.module.gamepieceimage;

import java.awt.Font;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import VASSAL.configure.Configurer;
import VASSAL.tools.SequenceEncoder;

public class FontStyleConfigurer extends Configurer {

  protected JPanel p;
  protected JPanel fontPanel;
  protected JComboBox fonts;

  public FontStyleConfigurer(String key, String name) {
    super(key, name);
  }

  public FontStyleConfigurer(String key, String name, FontStyle fontStyle) {
    this(key, name);
    setValue(fontStyle);
  }

  public FontStyleConfigurer(String key, String name, String styleName) {
    this(key, name, FontManager.getFontManager().getFontStyle(styleName));
  }

  public String getValueString() {
    return "";
  }

  public Font getValueFont() {
    return ((FontStyle) value).getFont();
  }

  public FontStyle getValueFontStyle() {
    return (FontStyle) value;
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      fontPanel = new JPanel();

      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      box.add(new JLabel(name));

      box.add(fontPanel);
      p.add(box);

    }
    buildFonts();
    return p;
  }

  protected void buildFonts() {
    if (fontPanel == null) {
      return;
    }

    if (fonts != null) {
      fontPanel.remove(fonts);
    }

    fonts = new JComboBox();
    String[] s = FontManager.getFontManager().getFontNames();
    for (int i = 0; i < s.length; i++) {
      fonts.addItem(s[i]);
    }
    fonts.setSelectedItem(value == null ? "Default" : ((FontStyle) value).getConfigureName()); //$NON-NLS-1$
    fontPanel.add(fonts);

    ItemListener l = new ItemListener() {
      public void itemStateChanged(ItemEvent evt) {
        updateValue();
      }
    };

    fonts.addItemListener(l);

  }

  protected void updateValue() {
    setValue(FontManager.getFontManager().getFontStyle((String) fonts.getSelectedItem()));
  }

  public void setValue(String s) {
    setValue(FontManager.getFontManager().getFontStyle(s));
    buildFonts();
  }

  public static FontStyle decode(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
    return new FontStyle(sd.nextToken("Default"), FontConfigurer.decode(sd.nextToken(""))); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public static String encode(FontStyle f) {
    SequenceEncoder se = new SequenceEncoder(f.getConfigureName(), '|');
    se.append(FontConfigurer.encode(f.getFont()));
    return se.getValue();
  }
}
