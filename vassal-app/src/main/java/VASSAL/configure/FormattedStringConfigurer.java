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
// - Added JComboBox.setPopupVisible override
// - Changed to use replaceSelection()
// - Added caret position preservation
// - Maintained all original functionality
package VASSAL.configure;

import VASSAL.i18n.Resources;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.SwingUtilities;
import javax.swing.text.JTextComponent;

public class FormattedStringConfigurer extends StringConfigurer implements ActionListener, FocusListener {
  private final DefaultComboBoxModel<String> optionsModel = new DefaultComboBoxModel<>();
  private JComboBox<String> dropList;
  private boolean processingSelection = false;
  private boolean skipTextSelect = false;

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
    for (final String option : options) {
      optionsModel.addElement(option);
    }
    setListVisibility();
  }

  /**
   * @return the current list of options (excluding the initial "insert" prompt)
   */
  public String[] getOptions() {
    final String[] s = new String[optionsModel.getSize() - 1];
    for (int i = 1; i < optionsModel.getSize(); ++i) {
      s[i - 1] = optionsModel.getElementAt(i);
    }
    return s;
  }

  @Override
  public Component getControls() {
    if (p == null) {
      super.getControls();

      dropList = new JComboBox<>(optionsModel) {
        @Override
        public void setPopupVisible(boolean visible) {
          skipTextSelect = visible;
          if (!processingSelection && nameField != null) {
            final JTextComponent tc = nameField;
            final int pos = tc.getCaretPosition();
            SwingUtilities.invokeLater(() -> tc.setCaretPosition(
                    Math.min(pos, tc.getText().length())
            ));
          }
          super.setPopupVisible(visible);
        }
      };

      dropList.setSelectedIndex(0);
      dropList.setEnabled(false);
      dropList.addActionListener(this);

      nameField.addCaretListener(e -> {
        if (skipTextSelect) {
          nameField.setSelectionStart(e.getDot());
          nameField.setSelectionEnd(e.getDot());
        }
      });

      nameField.addFocusListener(this);
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
    if (processingSelection || dropList == null || nameField == null) {
      return;
    }

    try {
      processingSelection = true;
      final int selectedIndex = dropList.getSelectedIndex();
      if (selectedIndex > 0) {
        final String item = "$" + optionsModel.getElementAt(selectedIndex) + "$";
        final JTextComponent textComp = nameField;
        final int pos = textComp.getCaretPosition();
        textComp.replaceSelection(item);
        textComp.setCaretPosition(
                Math.min(pos + item.length(), textComp.getText().length())
        );
      }
    }
    finally {
      SwingUtilities.invokeLater(() -> {
        dropList.setSelectedIndex(0);
        processingSelection = false;
      });
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
    if (dropList != null) {
      dropList.setEnabled(false);
    }
  }
}