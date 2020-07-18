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
import javax.swing.Action;
import javax.swing.JTree;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.LockableChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SynchCommand;
import VASSAL.i18n.Resources;

/**
 * When invoked, will request synchronization info from another player
 *  - Cannot Synch when in the default room
 *  - Cannot Synch with a player in a different room
 *  - Cannot Synch with yourself
 *  - Cannot Synch with any player in the same room within 15 seconds of your last synch in this room
 */
public class SynchAction extends AbstractAction {
  private static final long serialVersionUID = 1L;
  private static final long TOO_SOON = 15 * 1000;
  private static Room lastRoom;
  private static long lastSync = System.currentTimeMillis();

  private Player p;
  private ChatServerConnection client;
  private Room targetRoom;

  public SynchAction(Player p, ChatServerConnection client) {
    super(Resources.getString("Chat.synchronize")); //$NON-NLS-1$
    this.p = p;
    this.client = client;

    // Find which room our target player is in
    targetRoom = null;
    for (Room room : client.getAvailableRooms()) {
      if (room.getPlayerList().contains(p)) {
        targetRoom = room;
      }
    }

    final long now = System.currentTimeMillis();

    setEnabled(
      p != null
      && GameModule.getGameModule() != null
      && !p.equals(client.getUserInfo())
      && client.getRoom() != null
      && client.getRoom().equals(targetRoom)
      && (!targetRoom.equals(lastRoom) || (now - lastSync) > TOO_SOON));
  }

  public static void clearSynchRoom() {
    lastRoom = null;
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    if (isEnabled()) {
      final long now = System.currentTimeMillis();
      if ( ! targetRoom.equals(lastRoom) || (now - lastSync) > TOO_SOON) {
        GameModule.getGameModule().getGameState().setup(false);
        client.sendTo(p, new SynchCommand(client.getUserInfo(),client));
        lastSync = now;
      }
      lastRoom = targetRoom;
    }
  }


  public static PlayerActionFactory factory(final ChatServerConnection client) {
    return new PlayerActionFactory() {
      @Override
      public Action getAction(SimplePlayer p, JTree tree) {
        final Room r = client.getRoom();
        if (client instanceof LockableChatServerConnection && ((LockableChatServerConnection) client).isDefaultRoom(r)) {
          return null;
        }
        return new SynchAction(p,client);
      }
    };
  }
}
