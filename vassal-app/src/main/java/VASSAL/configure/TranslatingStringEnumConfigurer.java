/*
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

import VASSAL.i18n.Resources;
import java.awt.Component;
import java.awt.Dimension;
import javax.swing.Box;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;

/**
 * A Configurer that allows a user to select from an underlying list
 * of values, based on a translatable list of descriptions of those values.
 *
 * Drop-in replacement for {@link StringEnumConfigurer} that allows a set of
 * translation keys to be supplied to specify the drop-down values
 */
public class TranslatingStringEnumConfigurer extends Configurer {
  private final String[] validValues;
  private final String[] i18nKeys;
  private JComboBox<String> box;
  private Box panel;

  /**
   * @param key Configurer Key
   * @param name Configurer Name
   * @param validValues List of values to maintain
   * @param i18nKeys List of Trnaslation Keys used to describe the list of values
   */
  public TranslatingStringEnumConfigurer(String key, String name, String[] validValues, String[] i18nKeys) {
    super(key, name, validValues);
    this.validValues = validValues;
    this.i18nKeys = i18nKeys;
  }

  @Override
  public Component getControls() {
    if (panel == null) {

      // Translate the keys based on the current locale
      final String[] displayValues = new String[i18nKeys.length];
      for (int i = 0; i < i18nKeys.length; i++) {
        displayValues[i] = Resources.getString(i18nKeys[i]);
      }

      panel = Box.createHorizontalBox();
      panel.add(new JLabel(name));
      box = new JComboBox<>(displayValues);
      box.setMaximumSize(new Dimension(box.getMaximumSize().width, box.getPreferredSize().height));
      if (isValidValue(getValue())) {
        box.setSelectedIndex(getValueIndex(getValue()));
      }
      else if (displayValues.length > 0) {
        box.setSelectedIndex(0);
      }
      box.addActionListener(e -> {
        final int selected = box.getSelectedIndex();
        if (selected >= 0) {
          noUpdate = true;
          // Convert the selected drop-down index back to a 'real' value from our list
          setValue(validValues[selected]);
          noUpdate = false;
        }
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
    for (String validValue : validValues) {
      if (validValue.equals(o)) {
        return true;
      }
    }
    return false;
  }

  public int getValueIndex(Object o) {
    for (int i = 0; i < validValues.length; ++i) {
      if (validValues[i].equals(o)) {
        return i;
      }
    }
    return 0;
  }

  public String[] getValidValues() {
    return validValues;
  }

  /**
   * Set a value into the configurer
   * If the value is one of the list of allowable values, then set the
   * core value and update the drop-down list to show the corresponding
   * translation.
   *
   * @param o Value to set
   */
  @Override
  public void setValue(Object o) {
    if (validValues == null || isValidValue(o)) {
      super.setValue(o);
      if (!noUpdate && box != null && validValues != null) {
        for (int i = 0; i < validValues.length; i++) {
          if (validValues[i].equals(o)) {
            box.setSelectedIndex(i);
            break;
          }
        }
      }
    }
  }

  @Override
  public String getValueString() {
    return getValue().toString();
  }

  @Override
  public void setValue(String s) {
    setValue((Object) s);
  }

  // TODO move test code to a manual unit test annotated with @Ignore
  public static void main(String[] args) {
    JFrame f = new JFrame();
    StringEnumConfigurer c = new StringEnumConfigurer(null, "Pick one: ", new String[]{"one", "two", "three"}); // NON-NLS
    c.addPropertyChangeListener(evt -> System.err.println(evt.getPropertyName() + " = " + evt.getNewValue()));
    f.add(c.getControls());
    f.pack();
    f.setVisible(true);
  }
}
