/*
 * $Id: LockableJabberRoomControls.java 4778 2009-01-09 22:55:24Z uckelman $
 *
 * Copyright (c) 2009 by Brent Easton
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
package VASSAL.chat.jabber;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JPopupMenu;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.HybridClient;
import VASSAL.chat.Room;
import VASSAL.chat.ui.LockableRoomControls;
import VASSAL.i18n.Resources;

public class LockableJabberRoomControls extends LockableRoomControls {
  
  public LockableJabberRoomControls(JabberClient client) {
    super((ChatServerConnection) client);
  }

  protected void addLockRoomAction(JPopupMenu popup, Room target) {
    JabberClient c = getJabberClient();
    if (c != null) {
      popup.add(new LockRoomAction((JabberRoom) target, c));
    }
  }
  
  private JabberClient getJabberClient() {
    JabberClient c = null;
    if (client instanceof JabberClient) {
      c = (JabberClient) client;
    }
    else if (client instanceof HybridClient
        && ((HybridClient) client).getDelegate() instanceof JabberClient) {
      c = (JabberClient) ((HybridClient) client).getDelegate();
    }
    return c;
  }
  
  class LockRoomAction extends AbstractAction {
    private static final long serialVersionUID = 1L; 

    private JabberClient client;
    private JabberRoom target;

    public LockRoomAction(JabberRoom target, JabberClient client) {
      super(target.isLocked() ? Resources.getString("Chat.unlock_room")
                              : Resources.getString("Chat.lock_room"));

      setEnabled(target.isOwnedByMe() && !target.getName().equals(client.getDefaultRoomName()));
      this.target = target;
      this.client = client;
    }

    public void actionPerformed(ActionEvent e) {
      client.lockRoom(target);
    }
  } 
}