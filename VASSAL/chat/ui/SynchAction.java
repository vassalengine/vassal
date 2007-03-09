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
import VASSAL.chat.Player;
import VASSAL.chat.SynchCommand;
import VASSAL.i18n.Resources;

/**
 * When invoked, will request synchronization info from another player
 */
public class SynchAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private Player p;
  private ChatServerConnection client;

  public SynchAction(Player p, ChatServerConnection client) {
    super(Resources.getString("Chat.synchronize")); //$NON-NLS-1$
    this.p = p;
    this.client = client;
    if (p != null
      && GameModule.getGameModule() != null
      && !p.equals(client.getUserInfo())) {
      setEnabled(true);
    }
    else {
      setEnabled(false);
    }
  }

  public void actionPerformed(ActionEvent evt) {
    if (isEnabled()) {
      GameModule.getGameModule().getGameState().setup(false);
      client.sendTo(p, new SynchCommand(client.getUserInfo(),client));
    }
  }
  
  public static PlayerActionFactory factory(final ChatServerConnection client) {
    return new PlayerActionFactory() {
      public Action getAction(Player p, JTree tree) {
        return new SynchAction(p,client);
      }
    };
  }
}
