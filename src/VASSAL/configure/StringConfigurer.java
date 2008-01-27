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

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * A Configurer for String values
 */
public class StringConfigurer extends Configurer {
  protected JPanel p;
  protected JTextField nameField;

  public StringConfigurer(String key, String name) {
    this(key, name, "");
  }

  public StringConfigurer(String key, String name, String val) {
    super(key, name, val);
  }

  public String getValueString() {
    return (String) value;
  }

  public void setValue(String s) {
    if (!noUpdate && nameField != null) {
      nameField.setText(s);
    }
    setValue((Object) s);
  }

  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      p.add(new JLabel(getName()));
      nameField = buildTextField();
      nameField.setMaximumSize
        (new java.awt.Dimension(nameField.getMaximumSize().width,
                                nameField.getPreferredSize().height));
      nameField.setText(getValueString());
      p.add(nameField);
      nameField.addKeyListener(new java.awt.event.KeyAdapter() {
        public void keyReleased(java.awt.event.KeyEvent evt) {
          noUpdate = true;
          setValue(nameField.getText());
          noUpdate = false;
        }
      });
    }
    return p;
  }

  protected JTextField buildTextField() {
    return new JTextField(12);
  }
}
