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

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JTree;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.LockableRoom;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.node.NodeClient;
import VASSAL.chat.node.NodeRoom;
import VASSAL.i18n.Resources;

/**
 * When invoked, will Kick another player out of his current room back to the Main Room.
 */
public class InviteAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private Player target;
  private ChatServerConnection client;


  public InviteAction(ChatServerConnection client, Player target) {
    super(Resources.getString("Chat.invite")); //$NON-NLS-1$
    this.target = target;
    this.client = client;
    boolean enabled = false;
 
    if (target != null) {
      if (GameModule.getGameModule() != null) {
        final Room room = client.getRoom();
        if (room instanceof LockableRoom && ((LockableRoom) room).isLocked()) {
          if (room instanceof NodeRoom) {
            final String owner = ((NodeRoom) room).getOwner();
            if (owner.equals(client.getUserInfo().getId()) && !owner.equals(target.getId())) {
              enabled = true;
            }
          }
        }
      }
    }
    setEnabled(enabled);
  }

  public void actionPerformed(ActionEvent evt) {
    if (isEnabled()) {
      if (client instanceof NodeClient) {        
        ((NodeClient) client).sendInvite(target);
        GameModule.getGameModule().warn(Resources.getString("Chat.invite_sent", target.getName()));
      }
    }
  }
  
  public static PlayerActionFactory factory(final ChatServerConnection client) {
    return new PlayerActionFactory() {
      public Action getAction(Player p, JTree tree) {
        return new InviteAction(client, p);
      }
    };
  }
}
