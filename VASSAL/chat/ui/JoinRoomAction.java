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
package VASSAL.chat.ui;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Room;
import javax.swing.*;
import java.awt.event.ActionEvent;

/**
 * When invoked, will join a game room on the server
 */
public class JoinRoomAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private Room r;
  private ChatServerConnection client;

  public JoinRoomAction(Room r, ChatServerConnection client) {
    super("Join Room");
    this.r = r;
    this.client = client;
    setEnabled(r != null && !r.equals(client.getRoom()));
  }

  public void actionPerformed(ActionEvent evt) {
    client.setRoom(r);
  }
}
