/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Joel Uckelman, Brent Easton
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

import java.awt.Frame;

import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import net.miginfocom.swing.MigLayout;
import VASSAL.i18n.Resources;

/**
 * A window that displays information on a {@link VASSAL.chat.SimplePlayer}
 */
public class PlayerInfoWindow extends JDialog {
  private static final long serialVersionUID = 1L;

  public PlayerInfoWindow(Frame f, SimplePlayer p) {
    super(f, p.getName());
    setLayout(new MigLayout("insets dialog", "[align right][fill,grow]", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    // player name
    final JTextField name_f = new JTextField(p.getName().length());
    name_f.setText(p.getName());
    name_f.setEditable(false);

    final JLabel name_l = new JLabel(Resources.getString("Chat.real_name")); //$NON-NLS-1$
    name_l.setLabelFor(name_f);
    add(name_l);
    add(name_f, "pushx, wrap"); //$NON-NLS-1$

    // IP address
    final String ip = ((SimpleStatus)p.getStatus()).getIp();
    if (ip != null && ip.length() > 0) {
      final JTextField ip_f = new JTextField();
      ip_f.setText(ip);
      ip_f.setEditable(false);

      final JLabel ip_l = new JLabel(Resources.getString("Chat.ip_address")); //$NON-NLS-1$
      ip_l.setLabelFor(ip_f);

      add(ip_l);
      add(ip_f, "pushx, wrap"); //$NON-NLS-1$
    }

    // client version
    String client = ((SimpleStatus)p.getStatus()).getClient();
    if (client == null || client.length() == 0) client = "< 3.1"; //$NON-NLS-1$

    final JTextField client_f = new JTextField();
    client_f.setText(client);
    client_f.setEditable(false);

    final JLabel client_l =
      new JLabel(Resources.getString("Chat.client_version")); //$NON-NLS-1$
    client_l.setLabelFor(client_f);

    add(client_l);
    add(client_f, "pushx, wrap"); //$NON-NLS-1$

    // module version
    final String moduleVersion =
      ((SimpleStatus)p.getStatus()).getModuleVersion();
    if (moduleVersion != null && moduleVersion.length() > 0) {
      final JTextField mver_f = new JTextField();
      mver_f.setText(moduleVersion);
      mver_f.setEditable(false);

      final JLabel mver_l =
        new JLabel(Resources.getString("Chat.module_version")); //$NON-NLS-1$
      mver_l.setLabelFor(mver_f);

      add(mver_l);
      add(mver_f, "pushx, wrap"); //$NON-NLS-1$
    }

    // module checksum
    final String csum = ((SimpleStatus)p.getStatus()).getCrc();
    if (csum != null && csum.length() > 0) {
      final JTextField csum_f = new JTextField();
      csum_f.setText(csum);
      csum_f.setEditable(false);

      final JLabel csum_l =
        new JLabel(Resources.getString("Chat.module_checksum")); //$NON-NLS-1$
      csum_l.setLabelFor(csum_f);

      add(csum_l);
      add(csum_f, "pushx, wrap"); //$NON-NLS-1$
    }

    // Combined checksum
    final String c2sum = ((SimpleStatus)p.getStatus()).getCombinedCrc();
    if (c2sum != null && ! c2sum.isEmpty()) {
      final JTextField csum_f = new JTextField();
      csum_f.setText(c2sum);
      csum_f.setEditable(false);

      final JLabel csum_l = new JLabel(Resources.getString("Chat.combined_checksum")); //$NON-NLS-1$
      csum_l.setLabelFor(csum_f);

      add(csum_l);
      add(csum_f, "pushx, wrap"); //$NON-NLS-1$
    }

    // looking for a game?
    final JCheckBox looking_b =
      new JCheckBox(Resources.getString("Chat.looking_for_a_game")); //$NON-NLS-1$
    looking_b.setSelected(((SimpleStatus) p.getStatus()).isLooking());
    looking_b.setEnabled(false);

    add(looking_b, "gap top unrel, align left, span, wrap"); //$NON-NLS-1$

    // away from keyboard?
    final JCheckBox away_b =
      new JCheckBox(Resources.getString("Chat.away_from_keyboard")); //$NON-NLS-1$
    away_b.setSelected(((SimpleStatus) p.getStatus()).isAway());
    away_b.setEnabled(false);

    add(away_b, "align left, span, wrap unrel"); //$NON-NLS-1$

    // personal info
    final JTextArea pinfo_a = new JTextArea();
    pinfo_a.setText(((SimpleStatus) p.getStatus()).getProfile());
    pinfo_a.setEditable(false);
    pinfo_a.setLineWrap(true);
    pinfo_a.setWrapStyleWord(true);

    final JScrollPane pinfo_s = new JScrollPane(pinfo_a);

    final JLabel pinfo_l =
      new JLabel(Resources.getString("Chat.personal_info")); //$NON-NLS-1$
    pinfo_l.setLabelFor(pinfo_s);

    add(pinfo_l, "align left, wrap"); //$NON-NLS-1$
    add(pinfo_s, "span, grow, push"); //$NON-NLS-1$

    pack();
  }
}
