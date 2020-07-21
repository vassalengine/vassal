/*
 *
 * Copyright (c) 2008-2009 by Brent Easton
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
package VASSAL.script;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import VASSAL.configure.Configurer;

/**
 * A Configurer for a String that enforces the string to be a valid
 * Java name
 */
public class JavaNameConfigurer extends Configurer {
  protected JPanel p;
  protected JTextField nameField;

  public JavaNameConfigurer(String key, String name) {
    this(key, name, "");
  }

  public JavaNameConfigurer(String key, String name, String val) {
    super(key, name, val);
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

  @Override
  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      p.add(new JLabel(getName()));
      nameField = buildTextField();
      nameField.setMaximumSize(
        new java.awt.Dimension(nameField.getMaximumSize().width, nameField.getPreferredSize().height));
      nameField.setText(getValueString());
      p.add(nameField);
      nameField.addKeyListener(new java.awt.event.KeyAdapter() {
        @Override
        public void keyReleased(java.awt.event.KeyEvent evt) {
          noUpdate = true;
          String v = nameField.getText();
          int caret = nameField.getCaretPosition();
          StringBuilder sb = new StringBuilder();
          for (int i = 0; i < v.length(); i++) {
            char c = v.charAt(i);
            if (c == '$' || c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
              sb.append(c);
            }
            else if (i > 0 && c >= '0' && c <= '9') {
              sb.append(c);
            }
          }
          String newString = sb.toString();
          setValue(newString);
          nameField.setText(newString);
          if (newString.length() < v.length()) {
            caret--;
          }
          nameField.setCaretPosition(caret);
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
