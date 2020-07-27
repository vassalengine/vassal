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
import VASSAL.chat.LockableChatServerConnection;
import VASSAL.chat.LockableRoom;
import VASSAL.chat.Room;
import VASSAL.chat.SimplePlayer;
import VASSAL.i18n.Resources;

/**
 * When invoked, will Kick another player out of his current room back to the Main Room.
 */
public class KickAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private SimplePlayer kickee;
  private LockableChatServerConnection client;


  public KickAction(LockableChatServerConnection client, SimplePlayer target) {
    super(Resources.getString("Chat.kick")); //$NON-NLS-1$
    this.kickee = target;
    this.client = client;
    setEnabled(client.isKickable(kickee));
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    if (isEnabled()) {
      client.doKick(kickee);
      GameModule.getGameModule().warn(Resources.getString("Chat.kick_sent", kickee.getName())); //$NON-NLS-1$
    }
  }

  public static PlayerActionFactory factory(final LockableChatServerConnection client) {
    return new PlayerActionFactory() {
      @Override
      public Action getAction(SimplePlayer p, JTree tree) {
        final Room r = client.getRoom();
        if (r instanceof LockableRoom && !((LockableRoom) r).isLocked()) {
          return null;
        }
        return new KickAction(client, p);
      }
    };
  }
}
