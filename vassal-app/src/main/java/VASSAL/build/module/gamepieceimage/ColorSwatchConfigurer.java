/*
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
import java.awt.event.ItemListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

public class ColorSwatchConfigurer extends Configurer {

  protected JPanel p;
  protected JPanel swatchPanel;
  protected JComboBox<String> swatches;
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

  @Override
  public String getValueString() {
    return "";
  } //NON-NLS

  public Color getValueColor() {
    return ((ColorSwatch) value).getColor();
  }

  public ColorSwatch getValueColorSwatch() {
    return (ColorSwatch) value;
  }

  @Override
  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      swatchPanel = new JPanel();

      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));

      final Box box = Box.createHorizontalBox();
      box.add(new JLabel(name));
      buildSwatches();

      box.add(swatchPanel);
      p.add(box);

      colorBox = Box.createHorizontalBox();
      config = new ColorConfigurer("", Resources.getString("Editor.ColorSwatchConfigurer.select_color")); //$NON-NLS-1$
      config.addPropertyChangeListener(e -> {
        final Color c = (Color) config.getValue();
        final ColorSwatch cs = ColorManager.getColorManager().getColorSwatch(c);
        setValue(cs);
        buildSwatches();
        updateValue();
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

    final ItemListener l = evt -> updateValue();

    swatches = new SwatchComboBox(l, ((ColorSwatch) value).getConfigureName());
    swatchPanel.add(swatches);

  }

  protected void updateValue() {
    final String s = (String) swatches.getSelectedItem();
    if (ColorManager.SELECT_COLOR.equals(s)) {
      setValue(ColorManager.getColorManager().getColorSwatch((Color) config.getValue()));
    }
    else {
      setValue(ColorManager.getColorManager().getColorSwatch(s));
    }
    repack();
  }

  @Override
  protected void repack() {
    colorBox.setVisible(((ColorSwatch) getValue()).getConfigureName().equals(ColorManager.SELECT_COLOR));
    final Window w = SwingUtilities.getWindowAncestor(colorBox);
    if (w != null) {
      w.pack();
    }
  }

  @Override
  public void setValue(String s) {
    super.setValue(new ColorSwatch(s));
    buildSwatches();
  }

  public static ColorSwatch decode(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, '|');
    return new ColorSwatch(sd.nextToken(), sd.nextColor(Color.WHITE));
  }

  public static String encode(ColorSwatch f) {
    final SequenceEncoder se = new SequenceEncoder(f.getConfigureName(), '|');
    se.append(ColorConfigurer.colorToString(f.getColor()));
    return se.getValue();
  }
}
