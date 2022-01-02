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

import javax.swing.JPopupMenu;
import javax.swing.JTree;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.LockableRoom;
import VASSAL.chat.Room;


/**
 * @author rkinney
 */
public abstract class LockableRoomControls extends RoomInteractionControlsInitializer {

  protected JoinRoomAction joinAction;

  public LockableRoomControls(ChatServerConnection client) {
    super(client);
  }

  @Override
  public void doubleClickRoom(Room room, JTree tree) {
    if (!(room instanceof LockableRoom)
        || !((LockableRoom) room).isLocked()) {
      super.doubleClickRoom(room, tree);
    }
  }

  @Override
  public JPopupMenu buildPopupForRoom(Room target, JTree tree) {
    final JPopupMenu popup = new JPopupMenu();
    addJoinRoomAction(popup, target);

    if (target instanceof LockableRoom) {
      addLockRoomAction(popup, target);
    }
    return popup;
  }

  protected void addJoinRoomAction(JPopupMenu popup, Room target) {
    joinAction = new JoinRoomAction(target, client);
    popup.add(joinAction);
    if (target instanceof LockableRoom) {
      final LockableRoom nr = (LockableRoom) target;
      if (nr.isLocked()) {
        joinAction.setEnabled(false);
      }
    }
  }

  protected abstract void addLockRoomAction(JPopupMenu popup, Room target);

}
