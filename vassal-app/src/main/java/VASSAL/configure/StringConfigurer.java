/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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
import java.awt.Component;
import java.awt.Dimension;

import java.awt.Graphics;
import java.awt.event.FocusListener;
import javax.swing.JComponent;
import javax.swing.JLayer;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.LayerUI;

/**
 * A Configurer for String values
 */
public class StringConfigurer extends Configurer {
  protected JPanel p;
  protected JTextField nameField;
  protected int length;
  protected static final int DEFAULT_LENGHTH = 16;

  /**
   * Base Constructor for StringConfigurer
   *
   * @param key Configurer Key - Not used for new-style configs
   * @param name Configurer Name (label)
   * @param val Initial Configurer value
   * @param length Configurer length
   * @param hint Hint text
   */
  public StringConfigurer(String key, String name, int length, String hint, String val) {
    super(key, name, val);
    this.length = length > 0 ? length : DEFAULT_LENGHTH;
    this.hint = hint;
  }

  public StringConfigurer(String key, String name, String val) {
    this(key, name, DEFAULT_LENGHTH, "", val);
  }

  public StringConfigurer(String key, String name, int length) {
    this (key, name, length, "", "");
  }

  public StringConfigurer(String key, String name) {
    this(key, name, "");
  }

  public StringConfigurer(String val) {
    this (null, "", val);
  }

  @Override
  public String getValueString() {
    return (String) value;
  }

  @Override
  public void setValue(String s) {
    if (!noUpdate && nameField != null) {
      nameField.setText(s);
    }
    setValue((Object) s);
  }

  protected String getGrowthConstraint() {
    return "growx"; // NON-NLS
  }

  @Override
  public void requestFocus() {
    if (nameField != null) {
      nameField.requestFocus();
    }
  }

  @Override
  public Component getControls() {
    if (p == null) {
      p = new ConfigurerPanel(getName(), "[fill,grow]0[0]", "[][fill,grow][]"); // NON-NLS

      nameField = buildTextField();
      nameField.setMaximumSize(new Dimension(
        nameField.getMaximumSize().width,
        nameField.getPreferredSize().height
      ));
      nameField.setText(getValueString());

      final LayerUI<JTextField> layerUI = new ConfigLayerUI(this);
      final JLayer<JTextField> layer = new JLayer<>(nameField, layerUI);
      p.add(layer, getGrowthConstraint()); // NON-NLS
      nameField.getDocument().addDocumentListener(new DocumentListener() {
        @Override
        public void insertUpdate(DocumentEvent e) {
          update();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
          update();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {}

        private void update() {
          noUpdate = true;
          setValue(nameField.getText());
          noUpdate = false;
        }
      });
    }
    return p;
  }

  protected JTextField buildTextField() {
    return new HintTextField(length, hint);
  }

  public void updateHint(String hint) {
    setHint(hint);
    ((HintTextField) nameField).setHint(hint);
  }

  @Override
  public void setLabelVisibile(boolean visible) {
    if (p instanceof ConfigurerPanel) {
      ((ConfigurerPanel) p).setLabelVisibility(visible);
    }
  }

  @Override
  public void setHighlighted(boolean highlighted) {
    super.setHighlighted(highlighted);
    getControls();
    nameField.setBackground(highlighted ? LIST_ENTRY_HIGHLIGHT_COLOR : Color.white);
    nameField.repaint();
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    super.addFocusListener(listener);
    getControls();
    nameField.addFocusListener(listener);
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    super.removeFocusListener(listener);
    getControls();
    nameField.removeFocusListener(listener);
  }

  // Use JLayer to outline the field in Red as the Unix LaF ignores TextField background colours
  private static class ConfigLayerUI extends LayerUI<JTextField> {
    private static final long serialVersionUID = 1L;
    private final Configurer parent;

    public ConfigLayerUI(Configurer parent) {
      this.parent = parent;
    }

    @Override
    public void paint(Graphics g, JComponent c) {
      super.paint(g, c);
      final Component cc = ((JLayer) c).getView();
      if (parent.isHighlighted()) {
        final Dimension d = cc.getSize();
        g.setColor(Color.red);
        g.drawRect(0, 0, d.width - 2, d.height - 2);
      }
    }
  }
}
