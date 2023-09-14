/*
 *
 * Copyright (c) 2023 by The VASSAL Development Team
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

import VASSAL.build.GameModule;
import VASSAL.build.module.font.FontOrganizer;
import VASSAL.build.module.font.VassalFont;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.StructuredComboBox;
import VASSAL.tools.swing.SwingUtils;

import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.util.ArrayList;
import java.util.List;

public class StructuredFontConfigurer extends FontConfigurer {

  /** A set of suggested font sizes for the Labeler */
  public static final Integer[] GENERAL_SIZES = {8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 24, 36, 48, 72};

  /** A Set of suggested font sizes for setting UI Font */
  public static final Integer[] UI_SIZES = {8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 24};

  public static final Font DEFAULT_FONT = new Font(FontOrganizer.VASSAL_SANSERIF_FONT, Font.PLAIN, 12);

  public static final String DEMO_TEXT = "The Quick Brown Fox Jumped Over The Lazy Dog 1234567890 [](){}<>";

  /** Show all Fonts available on current system (false) or ony Fonts supplied by Vassal or Module */
  private boolean moduleSpecific;

  /** Do not show options for Bold or italic */
  private boolean plainOnly;

  /** Only show a limited set of sizes and do not allow user to choose their own size. We do not want people setting Chat Window to 1000 pt. */
  private boolean limitedSizes;

  /** Gui Elements */
  private JPanel panel;
  private StructuredComboBox familyConfig;
  private JComboBox<Integer> fontSizeConfig;
  private JCheckBox boldConfig;
  private JCheckBox italicConfig;
  private JTextField viewer;

  /**
   *
   * @param key            Configurer Key
   * @param name           Configurer Name
   * @param font           Default value
   * @param moduleSpecific True to include only Vassal supplied fonts, module fonts and Java Logical fonts
   *                       False to include only Vassal supplied fonts, Java Logical fonts and Locally installed Fonts
   */

  private  StructuredFontConfigurer(String key, String name, Font font, boolean moduleSpecific, boolean plainOnly, boolean limitedSizes) {
    super(key, name, font, new Integer[0]);
    this.moduleSpecific = moduleSpecific;
    this.plainOnly = plainOnly;
    this.limitedSizes = limitedSizes;
  }

  public StructuredFontConfigurer(String key, String name, Font font) {
    this(key, name, font, false, false, false);
  }

  public StructuredFontConfigurer(String key, String name) {
    this(key, name, DEFAULT_FONT);
  }

  public static Font decode(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    final String fontFamily = sd.nextToken(DEFAULT_FONT.getFamily());
    final int fontSize = sd.nextInt(DEFAULT_FONT.getSize());
    final int fontStyle = sd.nextInt(DEFAULT_FONT.getStyle());
    return new Font(fontFamily, fontStyle, fontSize);
  }

  public static String encode(Font f) {
    final SequenceEncoder se = new SequenceEncoder(',');
    se.append(f.getFamily())
      .append(f.getSize())
      .append(f.getStyle());
    return se.getValue();
  }

  @Override
  public String getValueString() {
    return value == null ? null : encode((Font) value);
  }

  public String getValueFamily() {
    return value == null ? DEFAULT_FONT.getFamily() : ((Font) value).getFamily();
  }

  public int getValueStyle() {
    return value == null ? DEFAULT_FONT.getStyle() : ((Font) value).getStyle();
  }

  public int getValueSize() {
    return value == null ? DEFAULT_FONT.getSize() : ((Font) value).getSize();
  }

  public Font getValueFont() {
    return (Font) value;
  }

  public boolean isModuleSpecific() {
    return moduleSpecific;
  }

  public void setModuleSpecific(boolean moduleSpecific) {
    this.moduleSpecific = moduleSpecific;
  }

  public boolean isPlainOnly() {
    return plainOnly;
  }

  public void setPlainOnly(boolean plainOnly) {
    this.plainOnly = plainOnly;
  }

  public boolean isLimitedSizes() {
    return limitedSizes;
  }

  public void setLimitedSizes(boolean limitedSizes) {
    this.limitedSizes = limitedSizes;
  }

  @Override
  public void setValue(String s) {
    setValue(decode(s));
  }

  private String[] getStructuredFontFamilyList() {
    final FontOrganizer fo = GameModule.getGameModule().getFontOrganizer();
    final List<String> fontList = new ArrayList<>();
    boolean first = true;
    String lastValue = "";

    // Vassal supplied fonts
    if (!fo.getVassalFonts().isEmpty()) {
      fontList.add("-" + Resources.getString("Editor.FontConfigure.vassal_fonts"));
      first = false;
      for (final VassalFont font : fo.getVassalFonts()) {
        final String family = font.getFontFamily();
        if (!lastValue.equals(family)) {
          fontList.add(family);
        }
        lastValue = family;
      }
    }

    // Module supplied fonts
    if (moduleSpecific) {
      if (!fo.getModuleFonts().isEmpty()) {
        if (first) {
          first = false;
        }
        else {
          fontList.add("-");
        }
        fontList.add("-" + Resources.getString("Editor.FontConfigure.module_fonts"));
        lastValue = "";
        for (final VassalFont font : fo.getModuleFonts()) {
          final String family = font.getFontFamily();
          if (!lastValue.equals(family)) {
            fontList.add(family);
          }
          lastValue = family;
        }
      }
    }

    // Java Logical Fonts
    if (first) {
      first = false;
    }
    else {
      fontList.add("-");
    }
    fontList.add("-" + Resources.getString("Editor.FontConfigure.java_fonts"));
    fontList.add(Font.DIALOG);
    fontList.add(Font.DIALOG_INPUT);
    fontList.add(Font.MONOSPACED);
    fontList.add(Font.SANS_SERIF);
    fontList.add(Font.SERIF);

    // Local Fonts
    if (!moduleSpecific) {
      if (first) {
        first = false;
      }
      else {
        fontList.add("-");
      }
      fontList.add("-" + Resources.getString("Editor.FontConfigure.local_fonts"));
      for (final String fontName : GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames()) {
        fontList.add(fontName);
      }
    }

    return fontList.toArray(new String[0]);
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new ConfigurerPanel(getName(), "[]unrel[]rel[]unrel[]rel[]unrel[]rel[]", "[]unrel[]unrel[]rel[]unrel[]rel[]unrel[]rel[]");

      familyConfig = new StructuredComboBox(getStructuredFontFamilyList());
      familyConfig.setSelectedItem(getValueFamily());
      panel.add(familyConfig);


      final JLabel sizeLabel = new JLabel(Resources.getString("Editor.size_label"));
      fontSizeConfig = new JComboBox<>(isLimitedSizes() ? UI_SIZES : GENERAL_SIZES);
      fontSizeConfig.setEditable(!isLimitedSizes());
      fontSizeConfig.setSelectedItem(getValueSize());
      fontSizeConfig.setMaximumSize(new Dimension(50, fontSizeConfig.getPreferredSize().height));
      sizeLabel.setLabelFor(fontSizeConfig);
      panel.add(sizeLabel);
      panel.add(fontSizeConfig, plainOnly ? "wrap" : "");

      final JLabel boldLabel = new JLabel(Resources.getString("Editor.TextLabel.bold"));
      boldConfig = new JCheckBox();
      boldConfig.setSelected((getValueStyle() & Font.BOLD) != 0);
      boldLabel.setLabelFor(boldConfig);
      panel.add(boldLabel);
      panel.add(boldConfig);

      final JLabel italicLabel = new JLabel(Resources.getString("Editor.TextLabel.italic"));
      italicConfig = new JCheckBox();
      italicConfig.setSelected((getValueStyle() & Font.ITALIC) != 0);
      italicLabel.setLabelFor(italicConfig);
      panel.add(italicLabel);
      panel.add(italicConfig, "wrap");

      viewer = new JTextField(DEMO_TEXT);
      if (plainOnly) {
        panel.add(viewer, "span 3, grow");
      }
      else {
        panel.add(viewer, "span 7, grow");
      }

      familyConfig.addItemListener(e -> updateFamily());
      boldConfig.addActionListener(e -> updateStyle());
      italicConfig.addActionListener(e -> updateStyle());
      fontSizeConfig.addItemListener((e) -> updateFontSize());

      boldLabel.setVisible(!plainOnly);
      boldConfig.setVisible(!plainOnly);
      italicLabel.setVisible(!plainOnly);
      italicConfig.setVisible(!plainOnly);

      // Refresh the visualizer
      updateFontSize();
    }

    return panel;
  }

  @Override
  public void setLabelVisible(boolean visible) {
    if (panel instanceof ConfigurerPanel) {
      ((ConfigurerPanel) panel).setLabelVisibility(visible);
    }
  }

  private void updateFamily() {
    updateValue((String) familyConfig.getSelectedItem(), getValueStyle(), getValueSize());
  }

  private void updateFontSize() {
    final Object size = fontSizeConfig.getSelectedItem();
    if (size instanceof Integer) {
      updateValue(getValueFamily(), getValueStyle(), (Integer) size);
    }
    else {
      fontSizeConfig.setSelectedItem(getValueSize());
    }
  }

  private void updateStyle() {
    final int style = Font.PLAIN +
      (boldConfig.isSelected() ? Font.BOLD : 0) +
      (italicConfig.isSelected() ? Font.ITALIC : 0);
    updateValue(getValueFamily(), style, getValueSize());
  }

  private void updateValue(String family, int style, int size) {
    setValue(new Font(family, style, size));
    viewer.setFont((Font) getValue());
    viewer.revalidate();
    SwingUtils.repack(panel);
  }

}