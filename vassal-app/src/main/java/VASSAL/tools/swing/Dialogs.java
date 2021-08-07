/*
 *
 * Copyright (c) 2008 by Joel Uckelman
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
package VASSAL.tools.swing;

import java.awt.Component;
import java.awt.Font;

import javax.swing.GroupLayout;
import javax.swing.Icon;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.LayoutStyle;

import VASSAL.tools.BrowserSupport;
import VASSAL.tools.DialogUtils;

/**
 * Provides some basic kinds of dialogs.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class Dialogs {
  private Dialogs() {
  }

  public static void showMessageDialog(
    Component parent,
    String title,
    String heading,
    String message,
    int messageType) {

    showMessageDialog(parent, title, heading, message, messageType, null, null);
  }

  public static void showMessageDialog(
    Component parent,
    String title,
    String heading,
    String message,
    int messageType,
    Object key,
    String disableMsg) {

    showMessageDialog(parent, title, heading, message,
                      messageType, null, key, disableMsg);
  }

  public static void showMessageDialog(
    Component parent,
    String title,
    String heading,
    String message,
    int messageType,
    Icon icon,
    Object key,
    String disableMsg) {

    showDialog(parent, title, buildContents(heading, message), messageType,
               icon, JOptionPane.DEFAULT_OPTION, null, null, key, disableMsg);
  }

  public static int showConfirmDialog(
    Component parent,
    String title,
    String heading,
    String message,
    int messageType,
    int optionType) {

    return showConfirmDialog(parent, title, heading, message,
                             messageType, null, optionType, null, null);
  }

  public static int showConfirmDialog(
    Component parent,
    String title,
    String heading,
    String message,
    int messageType,
    int optionType,
    Object key,
    String disableMsg) {

    return showConfirmDialog(parent, title, heading, message, messageType,
                             null, optionType, key, disableMsg);
  }

  public static int showConfirmDialog(
      Component parent,
      String title,
      String heading,
      String message,
      int messageType,
      Icon icon,
      int optionType,
      Object key,
      String disableMsg) {

    final Object o = showDialog(parent, title, buildContents(heading, message),
      messageType, icon, optionType, null, null, key, disableMsg);

    if (!(o instanceof Integer))
      return JOptionPane.CLOSED_OPTION;
    else
      return (Integer) o;
  }

  public static Object showDialog(
      Component parent,
      String title,
      Component content,
      int messageType,
      Icon icon,
      int optionType,
      Object[] options,
      Object initialValue,
      Object key,
      String disableMsg) {

    // set up the "don't show again" check box, if applicable
    final JCheckBox disableCheck;

    if (key != null) {
      if (DialogUtils.isDisabled(key)) return null;

      disableCheck = new JCheckBox(disableMsg);

      final JPanel panel = new JPanel();
      final GroupLayout layout = new GroupLayout(panel);
      panel.setLayout(layout);

      layout.setAutoCreateGaps(true);
      layout.setAutoCreateContainerGaps(false);

      layout.setHorizontalGroup(
        layout.createParallelGroup(GroupLayout.Alignment.LEADING, true)
          .addComponent(content)
          .addComponent(disableCheck));

      layout.setVerticalGroup(
        layout.createSequentialGroup()
          .addComponent(content)
          .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED,
                           GroupLayout.DEFAULT_SIZE, Integer.MAX_VALUE)
          .addComponent(disableCheck));

      content = panel;
    }
    else {
      disableCheck = null;
    }

    // build the option pane and dialog
    final JOptionPane opt = new JOptionPane(
      content, messageType, optionType, icon, options, initialValue);
    final JDialog dialog = opt.createDialog(parent, title);

// FIXME: setModal() is obsolete. Use setModalityType() in 1.6+.
//    d.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);
    dialog.setModal(true);
    dialog.setLocationRelativeTo(parent);
    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.setResizable(true);
    dialog.pack();
    dialog.setVisible(true);

    if (disableCheck != null && disableCheck.isSelected()) {
      DialogUtils.setDisabled(key, true);
    }

    return opt.getValue();
  }

  /**
   * Creates dialog contents with the given title and description.
   *
   * @param title the title
   * @param description the description
   */
  private static Component buildContents(String title, String description) {
    final JLabel titleLabel = new JLabel(title);
    final Font f = titleLabel.getFont();
    titleLabel.setFont(f.deriveFont(Font.BOLD, f.getSize() * 1.2f));

    final FlowLabel descriptionLabel = new FlowLabel(description);
    descriptionLabel.addHyperlinkListener(BrowserSupport.getListener());

    final JPanel panel = new JPanel();
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutoCreateGaps(true);
    layout.setAutoCreateContainerGaps(false);

    layout.setHorizontalGroup(
      layout.createParallelGroup(GroupLayout.Alignment.LEADING, true)
        .addComponent(titleLabel)
        .addComponent(descriptionLabel));

    layout.setVerticalGroup(
      layout.createSequentialGroup()
        .addComponent(titleLabel)
        .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
        .addComponent(descriptionLabel));

    return panel;
  }

  public static void main(String[] args) {
    final String loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."; // NON-NLS

    showMessageDialog(
      null,
      "Message Dialog", // NON-NLS
      "This Is the Header", // NON-NLS
      loremIpsum,
      JOptionPane.INFORMATION_MESSAGE
    );

    showMessageDialog(
      null,
      "Message Dialog", // NON-NLS
      "This Is the Header", // NON-NLS
      loremIpsum,
      JOptionPane.INFORMATION_MESSAGE,
      Boolean.TRUE,
      "Don't show this again" // NON-NLS
    );

    showMessageDialog(
      null,
      "Message Dialog", // NON-NLS
      "This Is the Header", // NON-NLS
      loremIpsum,
      JOptionPane.INFORMATION_MESSAGE,
      Boolean.TRUE,
      "Don't show this again" // NON-NLS
    );

    showConfirmDialog(
      null,
      "Confirmation Dialog", // NON-NLS
      "This Is the Header", // NON-NLS
      loremIpsum,
      JOptionPane.INFORMATION_MESSAGE,
      JOptionPane.YES_NO_OPTION
    );

    System.exit(0);
  }
}
