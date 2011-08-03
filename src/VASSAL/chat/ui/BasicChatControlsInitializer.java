/*
 * $Id$
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
package VASSAL.chat.ui;

import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;

import VASSAL.build.module.ServerConnection;
import VASSAL.chat.ChatServerConnection;
import VASSAL.i18n.Resources;

/** Adds Connect/Disconnect button to the server controls toolbar */
public class BasicChatControlsInitializer implements ChatControlsInitializer {
  private Action connectAction;
  private Action disconnectAction;
  private ChatServerConnection client;
  private JButton connectButton;
  private JButton disconnectButton;
  private PropertyChangeListener connectionListener;

  public BasicChatControlsInitializer(ChatServerConnection client) {
    super();
    this.client = client;
  }

  public void initializeControls(final ChatServerControls controls) {
    JToolBar toolbar = controls.getToolbar();

    connectAction = new AbstractAction(
                          Resources.getString("Chat.connect")) {  //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent evt) {
        client.setConnected(true);
      }
    };

    URL imageURL = getClass().getResource("/images/connect.gif");  //$NON-NLS-1$
    if (imageURL != null) {
      connectAction.putValue(Action.SHORT_DESCRIPTION, connectAction.getValue(Action.NAME));
      connectAction.putValue(Action.NAME, "");  //$NON-NLS-1$
      connectAction.putValue(Action.SMALL_ICON, new ImageIcon(imageURL));
    }
    connectAction.setEnabled(true);

    disconnectAction = new AbstractAction(
                        Resources.getString("Chat.disconnect")) {  //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent evt) {
        client.setConnected(false);
      }
    };

    imageURL = getClass().getResource("/images/disconnect.gif");  //$NON-NLS-1$
    if (imageURL != null) {
      disconnectAction.putValue(Action.SHORT_DESCRIPTION, disconnectAction.getValue(Action.NAME));
      disconnectAction.putValue(Action.NAME, "");  //$NON-NLS-1$
      disconnectAction.putValue(Action.SMALL_ICON, new ImageIcon(imageURL));
    }

    disconnectAction.setEnabled(false);
    connectButton = toolbar.add(connectAction);
    disconnectButton = toolbar.add(disconnectAction);

    connectionListener = new PropertyChangeListener() {
      public void propertyChange(final PropertyChangeEvent evt) {
        SwingUtilities.invokeLater(new Runnable() {
          public void run() {
            boolean connected = Boolean.TRUE.equals(evt.getNewValue());
            connectAction.setEnabled(!connected);
            disconnectAction.setEnabled(connected);
            if (!connected) {
              controls.getRoomTree().setRooms(new VASSAL.chat.Room[0]);
              controls.getCurrentRoom().setRooms(new VASSAL.chat.Room[0]);
            }
          }
        });
      }
    };

    client.addPropertyChangeListener(
      ServerConnection.CONNECTED, connectionListener);
  }

  public void uninitializeControls(ChatServerControls controls) {
    controls.getToolbar().remove(connectButton);
    controls.getToolbar().remove(disconnectButton);
  }

}
