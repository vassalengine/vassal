/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.chat;

import javax.swing.*;

import VASSAL.i18n.Resources;

/**
 * A window that displays information on a {@link VASSAL.chat.SimplePlayer}
 */
public class PlayerInfoWindow extends JDialog {
  private static final long serialVersionUID = 1L;

  public PlayerInfoWindow(java.awt.Frame f, SimplePlayer p) {
    super(f, p.getName());
    setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    
    Box b = Box.createHorizontalBox();
    JTextField tf = new JTextField(p.getName().length());
    tf.setText(p.getName());
    tf.setEditable(false);
    tf.setMaximumSize(new java.awt.Dimension(tf.getMaximumSize().width,
                                             tf.getPreferredSize().height));
    b.add(new JLabel(Resources.getString("Chat.real_name"))); //$NON-NLS-1$
    b.add(tf);
    add(b);

    String client = ((SimpleStatus)p.getStatus()).getClient();
    if (client == null || client.length() == 0) {
      client = "< 3.1";
    }
    b = Box.createHorizontalBox();
    tf = new JTextField(client.length());
    tf.setText(client);
    tf.setEditable(false);
    tf.setMaximumSize(new java.awt.Dimension(tf.getMaximumSize().width,
                                             tf.getPreferredSize().height));
    b.add(new JLabel(Resources.getString("Chat.client_version"))); //$NON-NLS-1$
    b.add(tf);
    add(b);
    
    final String ip = ((SimpleStatus)p.getStatus()).getIp();
    if (ip != null && ip.length() > 0) {
      b = Box.createHorizontalBox();
      tf = new JTextField(ip.length());
      tf.setText(ip);
      tf.setEditable(false);
      tf.setMaximumSize(new java.awt.Dimension(tf.getMaximumSize().width,
                                             tf.getPreferredSize().height));
      b.add(new JLabel(Resources.getString("Chat.ip_address"))); //$NON-NLS-1$
      b.add(tf);
      add(b);
    }
    
    JCheckBox box = new JCheckBox(Resources.getString("Chat.looking_for_a_game")); //$NON-NLS-1$
    box.setSelected(((SimpleStatus)p.getStatus()).isLooking());
    box.setEnabled(false);
    add(box);

    box = new JCheckBox(Resources.getString("Chat.away_from_keyboard")); //$NON-NLS-1$
    box.setSelected(((SimpleStatus)p.getStatus()).isAway());
    box.setEnabled(false);
    add(box);

    add(new JLabel(Resources.getString("Chat.personal_info"))); //$NON-NLS-1$
    JTextArea ta = new JTextArea();
    ta.setText(((SimpleStatus)p.getStatus()).getProfile());
    ta.setEditable(false);
    add(new JScrollPane(ta));
  
    pack();
  }
}
