/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.swing;

import java.awt.Component;
import java.awt.Font;

import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;
import VASSAL.tools.DialogUtils;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class DetailsDialog {

  public static void showDialog(
    final Component parent,
    final String title,
    final String header,
    final String message,
    final String details,
    final String disableText,
    final String showText,
    final String hideText,
    final int messageType,
    final Object key)
  {
    // set a slightly larger, bold font for the header
    final JLabel headerLabel = new JLabel(header);
    final Font f = headerLabel.getFont();
    headerLabel.setFont(f.deriveFont(Font.BOLD, f.getSize()*1.2f));

    // put together the paragraphs of the message
    final FlowLabel messageLabel = new FlowLabel(message);

    // set up the details view
    final JTextArea detailsArea = new JTextArea(details, 25, 80);
    detailsArea.setEditable(false);
    detailsArea.setLineWrap(true);
    detailsArea.setTabSize(2);

    final JScrollPane detailsScroll = new JScrollPane(detailsArea);
    detailsScroll.setVisible(false);

    final DetailsButton detailsButton =
      new DetailsButton(showText, hideText, detailsScroll);
    detailsButton.setBuddy(messageLabel);

    // build the contents panel
    final JPanel panel = new JPanel();
    panel.setLayout(new MigLayout("hidemode 3", "",
      key != null ? "[]unrel[]unrel[]rel[]unrel[]" : "[]unrel[]unrel[]rel[]"));

    panel.add(headerLabel, "cell 0 0, growx, pushx");
    panel.add(messageLabel, "cell 0 1, growx, pushx");
    panel.add(detailsButton, "cell 0 2");
    panel.add(detailsScroll, "cell 0 3, grow, push");

    final JCheckBox disableCheck;

    if (key != null) {
      disableCheck = new JCheckBox(disableText);
      panel.add(disableCheck, "cell 0 4");
    }
    else {
      disableCheck = null;
    }

    final JDialog dialog = new JOptionPane(
      panel,
      messageType,
      JOptionPane.DEFAULT_OPTION
    ).createDialog(parent, title);

// FIXME: setModal() is obsolete. Use setModalityType() in 1.6+.
//    d.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);
    dialog.setModal(true);
    dialog.setResizable(true);
    dialog.setLocationRelativeTo(parent);
    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.pack();
    dialog.setVisible(true);

    if (disableCheck != null && disableCheck.isSelected()) {
      DialogUtils.setDisabled(key, true);
    }
  }

  public static void main(String[] args) {
    final String loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";

    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        DetailsDialog.showDialog(
          null,
          "Test",
          "Test Header",
          loremIpsum,
          loremIpsum,
          "Don't show this dialog again",
          "Show Details",
          "Hide Details",
          JOptionPane.WARNING_MESSAGE,
          true
        );

        DetailsDialog.showDialog(
          null,
          "Test",
          "Test Header",
          loremIpsum,
          loremIpsum,
          "Don't show this dialog again",
          "Show Details",
          "Hide Details",
          JOptionPane.ERROR_MESSAGE,
          null
        );
      }
    });
  }
}
