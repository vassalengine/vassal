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

import javax.swing.AbstractAction;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Room;
import VASSAL.i18n.Resources;

/**
 * When invoked, will join a game room on the server
 */
public class JoinRoomAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private final Room r;
  private final ChatServerConnection client;

  public JoinRoomAction(Room r, ChatServerConnection client) {
    super(Resources.getString("Chat.join_room")); //$NON-NLS-1$
    this.r = r;
    this.client = client;
    setEnabled(r != null && !r.equals(client.getRoom()));

    if (r != null) {
      GameModule.getGameModule().warn(Resources.getString("Chat.joining_room", r.getName()));

      if (Resources.getString("Chat.main_room").equals(r.getName())) {
        explainMainRoom();
      }
      else {
        GameModule.getGameModule().warn(Resources.getString("Chat.explain_joined_room"));
      }
    }
  }

  public static void explainMainRoom() {
    GameModule.getGameModule().warn(Resources.getString("Chat.explain_main_room"));
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    client.setRoom(r);
  }

  public static RoomActionFactory factory(final ChatServerConnection chatClient) {
    return (p, tree) -> new JoinRoomAction(p, chatClient);
  }
}
