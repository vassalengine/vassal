/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * Configurer for specifing key masks (CTRL, SHIFT, CTRL-ALT, etc.)
 */
public class KeyModifiersConfigurer extends Configurer implements KeyListener {
  private JPanel controls;
  private JTextField tf;

  public KeyModifiersConfigurer(String key, String name) {
    super(key, name, 0);
  }

  public Component getControls() {
    if (controls == null) {
      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.X_AXIS));
      controls.add(new JLabel(getName()));
      tf = new JTextField(8);
      tf.addKeyListener(this);
      controls.add(tf);
      setValue(getValue());
    }
    return controls;
  }

  public String getValueString() {
    Object val = getValue();
    return val != null ? val.toString() : "0";
  }

  public void setValue(String s) {
    if (s == null) {
      s = "0";
    }
    try {
      setValue(Integer.valueOf(s));
    }
    catch (NumberFormatException e) {
      // Set to null if saved value is not a number
      setValue(null);
    }
  }

  public void setValue(Object o) {
    if (!noUpdate && tf != null) {
      if (o instanceof Integer
          && ((Integer) o).intValue() != 0) {
        tf.setText(KeyEvent.getKeyModifiersText(((Integer) o).intValue()).toUpperCase());
      }
      else {
        tf.setText("");
      }
    }
    super.setValue(o);
  }

  public void keyReleased(KeyEvent e) {
  }

  public void keyTyped(KeyEvent e) {
  }

  public void keyPressed(KeyEvent e) {
    setValue(e.getModifiers());
  }
}
