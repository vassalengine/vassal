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
import java.awt.KeyboardFocusManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;

public class FormattedStringConfigurer
    extends StringConfigurer
    implements ActionListener, FocusListener {

  private final DefaultComboBoxModel<String> optionsModel;
  private JComboBox<String> dropList;
  private Component cachedControls = null; // Caching the controls

  public FormattedStringConfigurer(String key, String name) {
    this(key, name, new String[0]);
  }

  public FormattedStringConfigurer(String[] options) {
    this(null, "", options);
  }

  @SuppressWarnings("checkstyle:MissingJavadocMethod")
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
    if (cachedControls == null) {
      if (p == null) {
        super.getControls();
        nameField.addFocusListener(this);
        dropList = new JComboBox<>(optionsModel);
        dropList.setSelectedIndex(0);
        dropList.setEnabled(false);
        dropList.addActionListener(this);
        setListVisibility();
        p.add(dropList, "grow 0,right");
        cachedControls = p;
      }
      System.out.println("getControls: Controls created and cached.");
    }
    else {
      System.out.println("getControls: Controls retrieved from cache.");
    }
    return cachedControls;
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
  public void actionPerformed(ActionEvent arg0) {
    final String item;
    final int selectedIndex = dropList.getSelectedIndex();
    setDropdownActive(true); // Lock text selection out in StringConfigurer

    System.out.println("actionPerformed: Thread=" + Thread.currentThread().getId() + ", event=" + arg0);
    System.out.println("actionPerformed: nameField.hasFocus()=" + nameField.hasFocus());
    System.out.println("actionPerformed: dropList.isEnabled()=" + dropList.isEnabled());

    if (selectedIndex > 0) {
      item = "$" + optionsModel.getElementAt(selectedIndex) + "$";
      String work = nameField.getText();

      int pos = nameField.getCaretPosition();
      // Cut out any selected text
      if (nameField.getSelectedText() != null) {
        final int start = nameField.getSelectionStart();
        final int end = nameField.getSelectionEnd();
        work = work.substring(0, start) + work.substring(end);
        pos = start;
      }

      final String news = work.substring(0, pos) + item + work.substring(pos);
      nameField.setText(news);
      nameField.setCaretPosition(pos + item.length());

      // Update the text field and repaint it
      noUpdate = true;
      setValue(nameField.getText());
      noUpdate = false;
      nameField.repaint();
    }

    // Send focus back to text field
    nameField.requestFocusInWindow();
    setDropdownActive(false); // Reset the flag in StringConfigurer
  }

  /*
   * Focus gained on text field, so enable insert drop-down
   * and make sure it says 'Insert'
   */
  @Override
  public void focusGained(FocusEvent arg0) {
    System.out.println("focusGained: Thread=" + Thread.currentThread().getId());
    dropList.setSelectedIndex(0);
    dropList.setEnabled(true);
    System.out.println("focusGained: dropList.isEnabled()=" + dropList.isEnabled());
    System.out.println("focusGained: focus owner = " + KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner());
    dropList.repaint();
  }

  @Override
  public void focusLost(FocusEvent arg0) {
    System.out.println("focusLost: Thread=" + Thread.currentThread().getId());
    dropList.setEnabled(false);
    System.out.println("focusLost: dropList.isEnabled()=" + dropList.isEnabled());
    System.out.println("focusLost: focus owner = " + KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner());
  }
}
