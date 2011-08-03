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

import java.awt.Color;
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
import javax.swing.SwingUtilities;

import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.tools.SequenceEncoder;

public class ColorSwatchConfigurer extends Configurer {

  protected JPanel p;
  protected JPanel swatchPanel;
  protected JComboBox swatches;
  protected Box colorBox;
  protected ColorConfigurer config;

  public ColorSwatchConfigurer(String key, String name) {
    this(key, name, ColorSwatch.getDefaultSwatch());
  }

  public ColorSwatchConfigurer(String key, String name, ColorSwatch swatch) {
    super(key, name);
    setValue(swatch);
  }

  public ColorSwatchConfigurer(String key, String name, String swatchName) {
    this(key, name, ColorManager.getColorManager().getColorSwatch(swatchName));
  }

  public ColorSwatchConfigurer(String key, String name, Color color) {
    this(key, name, ColorManager.getColorManager().getColorSwatch(color));
  }

  public String getValueString() {
    return "";
  }

  public Color getValueColor() {
    return ((ColorSwatch) value).getColor();
  }

  public ColorSwatch getValueColorSwatch() {
    return (ColorSwatch) value;
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      swatchPanel = new JPanel();

      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      box.add(new JLabel(name));
      buildSwatches();

      box.add(swatchPanel);
      p.add(box);

      colorBox = Box.createHorizontalBox();
      config = new ColorConfigurer("", "Select Color  "); //$NON-NLS-1$
      config.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          Color c = (Color) config.getValue();
          ColorSwatch cs = ColorManager.getColorManager().getColorSwatch(c);
          setValue(cs);
          buildSwatches();
          updateValue();
        }
      });
      colorBox.add(config.getControls());
      p.add(colorBox);

      repack();

    }
    return p;
  }

  protected void buildSwatches() {
    if (swatchPanel == null) {
      return;
    }

    if (swatches != null) {
      swatchPanel.remove(swatches);
    }

    ItemListener l = new ItemListener() {
      public void itemStateChanged(ItemEvent evt) {
        updateValue();
      }
    };

    swatches = new SwatchComboBox(l, ((ColorSwatch) value).getConfigureName());
    swatchPanel.add(swatches);

  }

  protected void updateValue() {
    String s = (String) swatches.getSelectedItem();
    if (s.equals(ColorManager.SELECT_COLOR)) {
      setValue(ColorManager.getColorManager().getColorSwatch((Color) config.getValue()));
    }
    else {
      setValue(ColorManager.getColorManager().getColorSwatch(s));
    }
    repack();
  }

  protected void repack() {
    colorBox.setVisible(((ColorSwatch) getValue()).getConfigureName().equals(ColorManager.SELECT_COLOR));
    Window w = SwingUtilities.getWindowAncestor(colorBox);
    if (w != null) {
      w.pack();
    }
  }

  public void setValue(String s) {
    super.setValue(new ColorSwatch(s));
    buildSwatches();
  }

  public static ColorSwatch decode(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
    return new ColorSwatch(sd.nextToken(), sd.nextColor(Color.WHITE));
  }

  public static String encode(ColorSwatch f) {
    SequenceEncoder se = new SequenceEncoder(f.getConfigureName(), '|');
    se.append(ColorConfigurer.colorToString(f.getColor()));
    return se.getValue();
  }
}
