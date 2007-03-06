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


import javax.swing.*;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.PrivateChatManager;
import VASSAL.chat.PrivateChatter;
import java.awt.event.ActionEvent;

/**
 * When invoked, will open a private message window to another player
 */
public class PrivateMessageAction extends AbstractAction {
    private static final long serialVersionUID = 1L;

    private Player p;
    private PrivateChatManager mgr;

    public PrivateMessageAction(Player p, ChatServerConnection client, PrivateChatManager mgr) {
	super("Private Msg");
	this.p = p;
	this.mgr = mgr;
	setEnabled(p != null 
		   && client != null
		   && mgr != null 
		   && !p.equals(client.getUserInfo()));
    }
    public void actionPerformed(ActionEvent evt) {
	PrivateChatter chat = mgr.getChatterFor(p);
	java.awt.Window f = (java.awt.Window)chat.getTopLevelAncestor();
	f.setVisible(true);
	f.toFront();
    }
    
    public static PlayerActionFactory factory(final ChatServerConnection client, final PrivateChatManager chatMgr) {
      return new PlayerActionFactory() {
        public Action getAction(Player p, JTree tree) {
          return new PrivateMessageAction(p, client, chatMgr);
        }
      };
    }
}
