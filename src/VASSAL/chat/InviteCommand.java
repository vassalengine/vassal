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

import javax.swing.JOptionPane;

import VASSAL.build.GameModule;
import VASSAL.chat.node.NodeClient;
import VASSAL.command.Command;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.Dialogs;

/**
 * A {@link Command} that, when executed, sends game synchronization
 * information to a given {@link VASSAL.chat.SimplePlayer}
 * NB InviteCommand is not used by the Jabber Server/Client
 *
 */
public class InviteCommand extends Command {
  private String room;
  private String player;
  private String playerId;
  private ChatServerConnection client;

  public InviteCommand(String player, String playerId, String room, ChatServerConnection client) {
    this.player = player;
    this.playerId = playerId;
    this.room = room;
    this.client = client;
  }

  public InviteCommand(String player, String playerId, String room) {
    this(player, playerId, room, null);
  }

  public String getRoom() {
    return room;
  }

  public String getPlayer() {
    return player;
  }

  public String getPlayerId() {
    return playerId;
  }

  protected void executeCommand() {
    if (client instanceof NodeClient) {
      final int i = Dialogs.showConfirmDialog(
        GameModule.getGameModule().getFrame(),
        Resources.getString("Chat.invite_heading"), //$NON-NLS-1$
        Resources.getString("Chat.invite_heading"), //$NON-NLS-1$
        Resources.getString("Chat.invitation", player, room),  //$NON-NLS-1$
        JOptionPane.QUESTION_MESSAGE,
        null,
        JOptionPane.YES_NO_OPTION,
        "Invite"+playerId,  //$NON-NLS-1$
        Resources.getString("Chat.ignore_invitation") //$NON-NLS-1$
      );

      if (i == 0) {
        ((NodeClient) client).doInvite(playerId, room);
      }
    }
  }

  protected Command myUndoCommand() {
    return null;
  }

  public boolean isLoggable() {
    return false;
  }
}
