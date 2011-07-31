/*
 * $Id$
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
package VASSAL.chat.node;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JPopupMenu;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.HybridClient;
import VASSAL.chat.Room;
import VASSAL.chat.ui.LockableRoomControls;
import VASSAL.i18n.Resources;

public class LockableNodeRoomControls extends LockableRoomControls {

  public LockableNodeRoomControls(NodeClient client) {
    super((ChatServerConnection) client);
  }

  protected void addLockRoomAction(JPopupMenu popup, Room target) {
    NodeClient c = getNodeClient();
    if (c != null) {
      popup.add(new LockRoomAction((NodeRoom) target, c));
    }
  }

  private NodeClient getNodeClient() {
    NodeClient c = null;
    if (client instanceof NodeClient) {
      c = (NodeClient) client;
    }
    else if (client instanceof HybridClient
        && ((HybridClient) client).getDelegate() instanceof NodeClient) {
      c = (NodeClient) ((HybridClient) client).getDelegate();
    }
    return c;
  }

  protected void createRoom(String name) {
    Room existing = null;
    Room[] rooms = client.getAvailableRooms();
    for (int i = 0; existing == null && i < rooms.length; i++) {
      if (rooms[i].getName().equals(name)) {
        existing = rooms[i];
      }
    }
    NodeClient nodeClient = getNodeClient();
    if (existing instanceof NodeRoom) {
      // Join existing room if it is not locked
      if (!((NodeRoom) existing).isLocked()) {
        client.setRoom(existing);
      }
    }
    else if (existing == null
        && nodeClient != null) {
      // If running hierarchical server, create new room and set myself as the owner
      NodeRoom room = new NodeRoom(name);
      room.setOwner(nodeClient.getMyInfo().getId());
      client.setRoom(room);
      nodeClient.sendRoomInfo(room);
    }
    else {
      // Default behavior
      super.createRoom(name);
    }
  }

  class LockRoomAction extends AbstractAction {
    private static final long serialVersionUID = 1L;

    private NodeClient client;
    private NodeRoom target;

    public LockRoomAction(NodeRoom target, NodeClient client) {
      super(target.isLocked() ? Resources.getString("Chat.unlock_room")
                              : Resources.getString("Chat.lock_room"));
      setEnabled(client.getMyInfo().getId().equals(target.getOwner()) &&
                 !target.getName().equals(client.getDefaultRoomName()));
      this.target = target;
      this.client = client;
    }

    public void actionPerformed(ActionEvent e) {
      client.lockRoom(target);
    }
  }

}