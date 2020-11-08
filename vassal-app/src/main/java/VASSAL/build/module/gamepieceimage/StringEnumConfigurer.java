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
package VASSAL.build.module.gamepieceimage;

import java.awt.Component;
import java.awt.Dimension;

import javax.swing.Box;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;

import VASSAL.configure.Configurer;

/**
 * A Configurer that returns a String from among a list of possible values
 */
public class StringEnumConfigurer extends Configurer {
  protected String[] validValues;
  protected JComboBox<String> box;
  protected Box panel;

  public StringEnumConfigurer(String key, String name, String[] validValues) {
    super(key, name);
    this.validValues = validValues;
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = Box.createHorizontalBox();
      panel.add(new JLabel(name));
      box = getComboBox();
      if (isValidValue(getValue())) {
        box.setSelectedItem(getValue());
      }
      else if (validValues.length > 0) {
        box.setSelectedIndex(0);
      }
      box.addActionListener(e -> {
        noUpdate = true;
        setValue(box.getSelectedItem());
        noUpdate = false;
      });
      panel.add(box);
    }
    return panel;
  }

  public JComboBox<String> getComboBox() {
    final JComboBox<String> b = new JComboBox<>(validValues);
    b.setMaximumSize(new Dimension(b.getMaximumSize().width,
                                   b.getPreferredSize().height));
    return b;
  }

  public boolean isValidValue(Object o) {
    for (final String validValue : validValues) {
      if (validValue.equals(o)) {
        return true;
      }
    }
    return false;
  }

  public String[] getValidValues() {
    return validValues;
  }

  public void setValidValues(String[] s) {
    validValues = s;
    box.setModel(new DefaultComboBoxModel<>(validValues));
  }

  @Override
  public void setValue(Object o) {
    if (validValues == null
        || isValidValue(o)) {
      super.setValue(o);
      if (!noUpdate && box != null) {
        box.setSelectedItem(o);
      }
    }
  }

  @Override
  public String getValueString() {
    return box != null ? (String) box.getSelectedItem() : validValues[0];
  }

  @Override
  public void setValue(String s) {
    setValue((Object) s);
  }

  // move test code to manual unit test annotated with @Ignore
  public static void main(String[] args) {
    final JFrame f = new JFrame();
    final StringEnumConfigurer c = new StringEnumConfigurer(null, "Pick one: ", new String[]{"one", "two", "three"}); //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    c.addPropertyChangeListener(evt -> {
      System.err.println(evt.getPropertyName() + " = " + evt.getNewValue()); //$NON-NLS-1$
    });
    f.add(c.getControls());
    f.pack();
    f.setVisible(true);
  }
}
