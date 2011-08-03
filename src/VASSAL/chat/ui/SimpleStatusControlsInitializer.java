/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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
package VASSAL.chat.ui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JOptionPane;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleStatus;
import VASSAL.i18n.Resources;

public class SimpleStatusControlsInitializer implements ChatControlsInitializer {
  private ChatServerConnection client;
  private boolean includeLooking;
  private JButton lookingBox;
  private JButton awayButton;

  /**
   * Entry Point for P2P client - 'Looking for Game' does not make sense.
   */
  public SimpleStatusControlsInitializer(ChatServerConnection client, boolean includeLooking) {
    super();
    this.client = client;
    this.includeLooking = includeLooking;
  }

  public SimpleStatusControlsInitializer(ChatServerConnection client) {
    this(client, true);
  }

  public void initializeControls(final ChatServerControls controls) {
    URL imageURL;

    if (includeLooking) {
      lookingBox = new JButton(Resources.getString("Chat.looking_for_a_game")); //$NON-NLS-1$
      lookingBox.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          if (client != null) {
            final Player p = client.getUserInfo();
            SimpleStatus s = (SimpleStatus) p.getStatus();
            s = new SimpleStatus(!s.isLooking(),s.isAway(),s.getProfile(), s.getClient(), s.getIp(), s.getModuleVersion(), s.getCrc());
            client.setUserInfo(new SimplePlayer(p.getId(),p.getName(),s));
          }
        }
      });
      lookingBox.setSize(lookingBox.getMinimumSize());
      imageURL = getClass().getResource("/images/playerLooking.gif"); //$NON-NLS-1$
      if (imageURL != null) {
        lookingBox.setToolTipText(lookingBox.getText());
        lookingBox.setText(""); //$NON-NLS-1$
        lookingBox.setIcon(new ImageIcon(imageURL));
      }
    }

    awayButton = new JButton(Resources.getString("Chat.away_from_keyboard")); //$NON-NLS-1$
    awayButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        if (client != null) {
          final Player p = client.getUserInfo();
          SimpleStatus s = (SimpleStatus) p.getStatus();
          s = new SimpleStatus(s.isLooking(),true,s.getProfile(), s.getClient(), s.getIp(), s.getModuleVersion(), s.getCrc());
          client.setUserInfo(new SimplePlayer(p.getId(),p.getName(),s));
          JOptionPane.showMessageDialog(controls.getRoomTree(), Resources.getString("Chat.im_back"), Resources.getString("Chat.away_from_keyboard"), JOptionPane.PLAIN_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
          s = (SimpleStatus) p.getStatus();
          s = new SimpleStatus(s.isLooking(),false ,s.getProfile(), s.getClient(), s.getIp(), s.getModuleVersion(), s.getCrc());
          client.setUserInfo(new SimplePlayer(p.getId(),p.getName(),s));
        }
      }
    });
    imageURL = getClass().getResource("/images/playerAway.gif"); //$NON-NLS-1$
    if (imageURL != null) {
      awayButton.setToolTipText(awayButton.getText());
      awayButton.setText(""); //$NON-NLS-1$
      awayButton.setIcon(new ImageIcon(imageURL));
    }

    if (includeLooking) {
      controls.getToolbar().add(lookingBox);
    }
    controls.getToolbar().add(awayButton);
  }

  public void uninitializeControls(ChatServerControls controls) {
    if (includeLooking) {
      controls.getToolbar().remove(lookingBox);
    }
    controls.getToolbar().remove(awayButton);
  }
}
