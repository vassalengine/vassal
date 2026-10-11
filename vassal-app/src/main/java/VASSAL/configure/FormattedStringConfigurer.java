/*
 *
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
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;

public class FormattedStringConfigurer
    extends StringConfigurer
    implements ActionListener, FocusListener {

  private final DefaultComboBoxModel<String> optionsModel;
  private JComboBox<String> dropList;

  public FormattedStringConfigurer(String key, String name) {
    this(key, name, new String[0]);
  }

  public FormattedStringConfigurer(String[] options) {
    this(null, "", options);
  }

  public FormattedStringConfigurer(
      String key,
      String name,
      String[] options) {
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

      // Focus gained/lost on text field,
      // so enable/disable insert drop-down
      nameField.addFocusListener(new FocusListener() {
        @Override
        public void focusGained(FocusEvent event) {
          if (dropList != null) {
            dropList.setSelectedIndex(0);
            dropList.setEnabled(true);
          }
        }

        @Override
        public void focusLost(FocusEvent event) {
          if (dropList != null) {
            dropList.setPopupVisible(false);
            dropList.setEnabled(false);
          }
        }
      });

      // The insert key expands the insert drop-down.
      nameField.addKeyListener(new KeyListener() {
        @Override
        public void keyTyped(KeyEvent e) {
        }

        @Override
        public void keyPressed(KeyEvent e) {
        }

        @Override
        public void keyReleased(KeyEvent e) {
          final int code = e.getKeyCode();
          if (code == KeyEvent.VK_INSERT && dropList != null) {
            dropList.setPopupVisible(true);
          }
        }
      });

      dropList = new JComboBox<>(optionsModel);

      // Make dropdown not focusable to remove it from the tab control list.
      // This allows focus to move to the next control.
      dropList.setFocusable(false);
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

  /*
   * Drop-down list has been clicked, insert selected option onto string
   */
  @Override
  public void actionPerformed(ActionEvent e) {
    final int selectedIndex = dropList.getSelectedIndex();
    if (selectedIndex > 0) {
      final String item = "$" + optionsModel.getElementAt(selectedIndex) + "$";
      final int start = nameField.getSelectionStart();
      final int end = nameField.getSelectionEnd();
      final String text = nameField.getText();

      nameField.setText(text.substring(0, start) + item + text.substring(end));
      nameField.setCaretPosition(start + item.length());
    }
    dropList.setSelectedIndex(0);
    nameField.requestFocusInWindow();
  }

  @Override
  public void focusGained(FocusEvent arg0) {
    // nothing to do, handled by addKeyListener
  }

  @Override
  public void focusLost(FocusEvent arg0) {
    // nothing to do, handled by addKeyListener
  }
}
