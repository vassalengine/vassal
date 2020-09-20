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
import java.util.List;
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
   * Create a drop-list of localised display values that allows you to select a value from an
   * underlying list of 'internal' untranslated values.
   *
   * @param key Configurer Key
   * @param name Configurer Name
   * @param validValues Array of values to maintain
   * @param i18nKeys Array of Translation Keys used to describe the list of values
   */
  public TranslatingStringEnumConfigurer(String key, String name, String[] validValues, String[] i18nKeys) {
    super(key, name, validValues);
    this.validValues = validValues;
    this.i18nKeys = i18nKeys;
  }

  /**
   * Create a drop-list of localised display values that allows you to select a value from an
   * underlying list of 'internal' untranslated values.
   *
   * @param key Configurer Key
   * @param name Configurer Name
   * @param validValues List of values to maintain
   * @param i18nKeys List of Translation Keys used to describe the list of values
   */
  public TranslatingStringEnumConfigurer(String key, String name, List<String> validValues, List<String> i18nKeys) {
    this (key, name, validValues.toArray(new String[0]), i18nKeys.toArray(new String[0]));
  }

  /**
   * Create a drop-list of localised display values that allows you to select a value from an
   * underlying list of 'internal' untranslated values.
   *
   * @param key Configurer Key
   * @param name Configurer Name
   * @param validValues Array of values to maintain
   * @param i18nKeys Array of Translation Keys used to describe the list of values
   * @param initialValue Initial Value to set in the Configurer.
   */
  public TranslatingStringEnumConfigurer(String key, String name, String[] validValues, String[] i18nKeys, String initialValue) {
    this (key, name, validValues, i18nKeys);
    setValue(initialValue);
  }

  /**
   * Create a drop-list of localised display values that allows you to select a value from an
   * underlying list of 'internal' untranslated values.
   *
   * @param key Configurer Key
   * @param name Configurer Name
   * @param validValues List of values to maintain
   * @param i18nKeys List of Translation Keys used to describe the list of values
   * @param initialValue Initial Value to set in the Configurer.
   */
  public TranslatingStringEnumConfigurer(String key, String name, List<String> validValues, List<String> i18nKeys, String initialValue) {
    this (key, name, validValues, i18nKeys);
    setValue(initialValue);
  }

  /**
   * Get the Controls that make up this Configurer
   * @return A swing Component that holds all of the Configurer controls
   */
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

  /**
   * Set the enabled status of the Configurer controls
   * @param enabled set the Enabled status of the Configurer
   */
  public void setEnabled(boolean enabled) {
    box.setEnabled(enabled);
  }

  /**
   * Set whether or not the value showing in the Configurer can be changed by a user
   * @param editable Set the Editable status of the Configurer
   */
  public void setEditable(boolean editable) {
    box.setEditable(editable);
  }

  /**
   * Test if a supplied value is one of the underlying 'real' valued managed by the Configurer
   * @param o Value to test
   * @return true if the supplied Object is one of the allowed values
   */
  public boolean isValidValue(Object o) {
    for (String validValue : validValues) {
      if (validValue.equals(o)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Return the index of the supplied value in the list of allowed values.
   * Return 0 if the supplied value is not found.
   * @param o Value to look for
   * @return The index of the value, or 0 (i.e. first) if not found
   */
  public int getValueIndex(Object o) {
    for (int i = 0; i < validValues.length; ++i) {
      if (validValues[i].equals(o)) {
        return i;
      }
    }
    return 0;
  }

  /**
   * Return the array of valid underlying values.
   * @return Array of underlying values.
   */
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

  /**
   * Return the underlying value stored in the Configurer
   * @return stored value
   */
  @Override
  public String getValueString() {
    return getValue().toString();
  }

  /**
   * Set the value stored in the Configurer.
   * Note that the value must be one of the valid allowed values.
   * @param s Value to store as Configurer value
   */
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
