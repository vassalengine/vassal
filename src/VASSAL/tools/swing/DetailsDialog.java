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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.jdesktop.layout.GroupLayout;
import org.jdesktop.layout.LayoutStyle;

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
    final int messageType,
    final Object key)
  {
    final JPanel panel = new JPanel();

    // set a slightly larger, bold font for the header
    final JLabel headerLabel = new JLabel(header);
    final Font f = headerLabel.getFont();
    headerLabel.setFont(f.deriveFont(Font.BOLD, f.getSize()*1.2f));

    // put together the paragraphs of the message
   final FlowLabel messageLabel = new FlowLabel(message);

    final JCheckBox disableCheck;

    // set up the details view
    final JTextArea detailsArea = new JTextArea(details, 10, 36);
    detailsArea.setEditable(false);

    final JScrollPane detailsScroll = new JScrollPane(detailsArea);
    detailsScroll.setVisible(false);

    // layout the panel  
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(false);

    if (key != null) {
      disableCheck = new JCheckBox("Do not show this dialog again");

      layout.setHorizontalGroup(
        layout.createParallelGroup(GroupLayout.LEADING, true)
          .add(headerLabel)
          .add(messageLabel)
          .add(detailsScroll)
          .add(disableCheck));
  
      layout.setVerticalGroup(
        layout.createSequentialGroup()
          .add(headerLabel)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(messageLabel, GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(detailsScroll, 0,
                              GroupLayout.PREFERRED_SIZE,
                              Integer.MAX_VALUE)
          .add(disableCheck));
    }
    else {
      disableCheck = null;

      layout.setHorizontalGroup(
        layout.createParallelGroup(GroupLayout.LEADING, true)
          .add(headerLabel)
          .add(messageLabel)
          .add(detailsScroll));
  
      layout.setVerticalGroup(
        layout.createSequentialGroup()
          .add(headerLabel)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(messageLabel, GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(detailsScroll, 0,
                              GroupLayout.PREFERRED_SIZE,
                              Integer.MAX_VALUE));
    }

    final JButton okButton = new JButton();
    final JButton detailsButton = new JButton();

    final JDialog dialog = new JOptionPane(
      panel,
      messageType,
      JOptionPane.DEFAULT_OPTION,
      null,
      new JButton[] { okButton, detailsButton }
    ).createDialog(parent, title);

    okButton.setAction(new AbstractAction("Ok") {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        dialog.dispose();
      }
    });

    detailsButton.setAction(new AbstractAction("<html>Details &raquo;</html>") {
      private static final long serialVersionUID = 1L;
    
      public void actionPerformed(ActionEvent e) {
        detailsScroll.setVisible(!detailsScroll.isVisible());
        putValue(NAME, detailsScroll.isVisible() ?
          "<html>Details &laquo;</html>" : "<html>Details &raquo;</html>");

        // ensure that neither expansion nor collapse changes the dialog width
        final Dimension d = messageLabel.getSize();
        d.height = Integer.MAX_VALUE;
        detailsScroll.setMaximumSize(d);
        messageLabel.setMaximumSize(d);

        dialog.pack();

        detailsScroll.setMaximumSize(null);
        messageLabel.setMaximumSize(null);
      }
    });
 
// FIXME: setModal() is obsolete. Use setModalityType() in 1.6+.
//    d.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);
    dialog.setModal(true);
    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.setResizable(true);
    dialog.pack();
    dialog.setVisible(true);

    if (disableCheck != null && disableCheck.isSelected()) {
      DialogUtils.setDisabled(key, true);
    }
  }
}
