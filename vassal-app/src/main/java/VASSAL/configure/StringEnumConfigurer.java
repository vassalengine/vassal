/*
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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
import java.awt.Dimension;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;

/**
 * A Configurer that returns a String from among a list of possible values
 */
public class StringEnumConfigurer extends Configurer {
  private String[] validValues;
  private JComboBox<String> box;
  private ConfigurerPanel panel;

  public StringEnumConfigurer(String key, String name, String[] validValues) {
    super(key, name);
    this.validValues = validValues;
  }

  public JComboBox<String> getBox() {
    return box;
  }

  public void setBox(JComboBox<String> box) {
    this.box = box;
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new ConfigurerPanel(getName(), "[]", "[]rel[]"); // NON-NLS

      box = new JComboBox<>(validValues);
      box.setMaximumSize(new Dimension(box.getMaximumSize().width, box.getPreferredSize().height));
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

  public void setEnabled(boolean enabled) {
    box.setEnabled(enabled);
  }

  public void setEditable(boolean enabled) {
    box.setEditable(enabled);
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
    if (box == null) {
      getControls();
    }
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
    if (box != null) {
      return (String) box.getSelectedItem();
    }
    else {
      return validValues.length > 0 ? validValues[0] : "";
    }
  }

  @Override
  public void setValue(String s) {
    setValue((Object) s);
  }

  @Override
  public void setLabelVisibile(boolean visible) {
    panel.setLabelVisibility(visible);
  }
}
