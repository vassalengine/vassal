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

public class FormattedStringConfigurer
        extends StringConfigurer
        implements ActionListener, FocusListener {

  private final DefaultComboBoxModel<String> optionsModel;
  private JComboBox<String> dropList;
  private boolean processingSelection = false;

  public FormattedStringConfigurer(String key, String name) {
    this(key, name, new String[0]);
  }

  public FormattedStringConfigurer(String[] options) {
    this(null, "", options);
  }

  public FormattedStringConfigurer(String key, String name, String[] options) {
    super(key, name);
    optionsModel = new DefaultComboBoxModel<>();
    setOptions(options);
  }

  public void setOptions(String[] options) {
    optionsModel.removeAllElements();
    optionsModel.addElement(Resources.getString("Editor.FormattedStringConfigurer.insert"));
    for (final String option : options) {
      optionsModel.addElement(option);
    }
    setListVisibility();
  }

  public String[] getOptions() {
    final String[] s = new String[optionsModel.getSize()];
    for (int i = 0; i < s.length; ++i) {
      s[i] = optionsModel.getElementAt(i);
    }
    return s;
  }

  @Override
  public Component getControls() {
    if (p == null) {
      super.getControls();

      nameField.addFocusListener(this);

      dropList = new JComboBox<>(optionsModel) {
        @Override
        public void setPopupVisible(boolean visible) {
          if (!processingSelection) {
            super.setPopupVisible(visible);
          }
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

  private void setListVisibility() {
    if (dropList != null) {
      dropList.setVisible(optionsModel.getSize() > 1);
    }
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    if (dropList.isPopupVisible()) {
      processingSelection = true;
      try {
        final int selectedIndex = dropList.getSelectedIndex();
        if (selectedIndex > 0) {
          String item = "$" + optionsModel.getElementAt(selectedIndex) + "$";

          // Get current selection/caret info before any focus changes
          JTextComponent textComp = nameField;
          int start = textComp.getSelectionStart();
          int end = textComp.getSelectionEnd();
          String text = textComp.getText();

          // Insert the new text
          String newText = text.substring(0, start) + item + text.substring(end);
          textComp.setText(newText);

          // Position caret after inserted text
          textComp.setCaretPosition(start + item.length());

          // Update value without triggering recursive updates
          noUpdate = true;
          setValue(newText);
          noUpdate = false;
        }
      } finally {
        processingSelection = false;
        dropList.setSelectedIndex(0);
      }

      // Return focus without triggering full selection
      nameField.requestFocusInWindow();
      nameField.setCaretPosition(nameField.getCaretPosition());
    }
  }

  @Override
  public void focusGained(FocusEvent e) {
    if (dropList != null) {
      dropList.setSelectedIndex(0);
      dropList.setEnabled(true);
    }
  }

  @Override
  public void focusLost(FocusEvent e) {
    if (dropList != null && !dropList.isFocusOwner() && !processingSelection) {
      dropList.setEnabled(false);
    }
  }
}