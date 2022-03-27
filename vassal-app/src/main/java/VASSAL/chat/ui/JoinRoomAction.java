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

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Room;
import VASSAL.chat.node.NodeClient;
import VASSAL.chat.node.NodeRoom;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.Dialogs;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.AbstractAction;
import javax.swing.JOptionPane;

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
  }

  public static void explainMainRoom() {
    GameModule.getGameModule().warn(Resources.getString("Chat.explain_main_room"));
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    if (client instanceof NodeClient) {
      final List<String> errors = new ArrayList<>();
      final boolean compatible = ((NodeClient) client).checkCompatibility((NodeRoom) r, errors);
      if (!compatible) {
        final StringBuilder sb = new StringBuilder();
        sb.append(Resources.getString("Chat.join_issue", r.getName())).append("\n\n");
        for (final String error : errors) {
          sb.append(" - ").append(error).append('\n');
        }
        sb.append('\n').append(Resources.getString("Chat.are_you_sure"));
        final int result =
          Dialogs.showConfirmDialog(GameModule.getGameModule().getPlayerWindow(),
            Resources.getString("Chat.vassal_configuration_error"),
            Resources.getString("Chat.vassal_configuration_error"),
            sb.toString(),
            JOptionPane.WARNING_MESSAGE,
            JOptionPane.YES_NO_OPTION);
        if (result != JOptionPane.YES_OPTION) {
          return;
        }
      }
    }

    client.setRoom(r);
    if (r != null) {
      final GameModule gm = GameModule.getGameModule();
      gm.warn(Resources.getString("Chat.joining_room", r.getName()));

      if (Resources.getString("Chat.main_room").equals(r.getName())) {
        explainMainRoom();
      }
      else {
        gm.warn(Resources.getString("Chat.explain_joined_room"));
      }
    }
  }

  public static RoomActionFactory factory(final ChatServerConnection chatClient) {
    return (p, tree) -> new JoinRoomAction(p, chatClient);
  }
}
