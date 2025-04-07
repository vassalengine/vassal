/*
 * Copyright (c) 2000-2009 by Rodney Kinney & Brent Easton
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

/*
 * FormattedStringConfigurer.
 * Extended version of StringConfigure that provides a drop down list of options that can
 * be inserted into the string
 */
// @generated-by: DeepSeek Chat (2024-06-20)
// @change-notes:
// - Stable dropdown behavior with skipFocusLoss
// - Proper finally block in action handler
// - Preserved all original functionality
package VASSAL.configure;

import VASSAL.i18n.Resources;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.text.JTextComponent;

public class FormattedStringConfigurer extends StringConfigurer implements ActionListener, FocusListener {
  private final DefaultComboBoxModel<String> optionsModel = new DefaultComboBoxModel<>();
  private JComboBox<String> dropList;

  // @deepseek-added: Tracks popup state to prevent focus issues
  private boolean skipFocusLoss;

  public FormattedStringConfigurer(String key, String name) {
    this(key, name, new String[0]);
  }

  public FormattedStringConfigurer(String[] options) {
    this(null, "", options);
  }

  public FormattedStringConfigurer(String key, String name, String[] options) {
    super(key, name);
    setOptions(options);
  }

  /**
   * Set the list of options available for insertion
   * @param options array of options
   */
  public void setOptions(String[] options) {
    optionsModel.removeAllElements();
    optionsModel.addElement(Resources.getString("Editor.FormattedStringConfigurer.insert"));
    if (options != null) {
      for (final String option : options) {
        optionsModel.addElement(option);
      }
    }
    setListVisibility();
  }

  /**
   * @return the current list of options (excluding the initial "insert" prompt)
   */
  public String[] getOptions() {
    final String[] s = new String[optionsModel.getSize() - 1];
    for (int i = 0; i < s.length; ++i) {
      s[i] = optionsModel.getElementAt(i + 1);
    }
    return s;
  }

  @Override
  // @deepseek-modified: Custom dropdown behavior
  public Component getControls() {
    if (p == null) {
      super.getControls();

      nameField.addFocusListener(this);

      dropList = new JComboBox<>(optionsModel) {
          // @deepseek-modified: Override popup visibility handling
          @Override
          public void setPopupVisible(boolean visible) {
              skipFocusLoss = visible;
              super.setPopupVisible(visible);
          }
      };
      dropList.setSelectedIndex(0);
      dropList.setEnabled(false);
      dropList.addActionListener(this);

      setListVisibility();
      p.add(dropList, "grow 0,right"); // NON-NLS
    }
    return p;
  }

  /**
   * Show the dropdown list only when there are options to select
   */
  private void setListVisibility() {
    if (dropList != null) {
      dropList.setVisible(optionsModel.getSize() > 1);
    }
  }

  /*
   * Drop-down list has been clicked, insert selected option onto string
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    if (e.getSource() == dropList) {
      try {
        final int selectedIndex = dropList.getSelectedIndex();
        if (selectedIndex > 0) {
          final String item = "$" + optionsModel.getElementAt(selectedIndex) + "$";
          final JTextComponent textComp = nameField;
          final int start = textComp.getSelectionStart();
          final int end = textComp.getSelectionEnd();
          final String text = textComp.getText();

          textComp.setText(text.substring(0, start) + item + text.substring(end));
          textComp.setCaretPosition(start + item.length());
        }
      }
      finally {
        // @deepseek-critical: Ensure cleanup happens
        dropList.setSelectedIndex(0);
        nameField.requestFocusInWindow();
        skipFocusLoss = false;
      }
    }
  }

  /*
   * Focus gained on text field, so enable insert drop-down
   * and make sure it says 'Insert'
   */
  @Override
  public void focusGained(FocusEvent e) {
    if (dropList != null) {
      dropList.setSelectedIndex(0);
      dropList.setEnabled(true);
    }
  }

  /*
   * Focus lost on text field, so disable insert drop-down
   */
  @Override
  public void focusLost(FocusEvent e) {
    if (dropList != null && !skipFocusLoss) {
      dropList.setEnabled(false);
    }
  }
}