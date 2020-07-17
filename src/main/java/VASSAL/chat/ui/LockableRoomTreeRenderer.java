/*
 *
 * Copyright (c) 2004-2009 by Rodney Kinney, Brent Easton
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

import java.awt.Component;
import java.awt.Font;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.LockableRoom;
import VASSAL.chat.Player;
import VASSAL.chat.Room;

/**
 * Renders rooms with a "locked" icon if locked
 * Change Owners name to display Red
 */
public class LockableRoomTreeRenderer extends RoomTreeRenderer {
  private static final long serialVersionUID = 1L;
  private Icon lockedIcon;
  private Font nonOwnerFont = null;
  private Font ownerFont = null;
  public LockableRoomTreeRenderer() {
    URL image = getClass().getResource("/images/lockedRoom.gif"); //$NON-NLS-1$
    if (image != null) {
      lockedIcon = new ImageIcon(image);
    }
  }

  @Override
  public Component getTreeCellRendererComponent(JTree tree, Object value,
                                                boolean sel,
                                                boolean expanded,
                                                boolean leaf, int row,
                                                boolean hasFocus) {
    JLabel l =  (JLabel) super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
    Object item = ((DefaultMutableTreeNode) value).getUserObject();

    if (item instanceof LockableRoom) {
      if (lockedIcon != null && ((LockableRoom)item).isLocked()) {
        l.setIcon(lockedIcon);
      }
    }
    else if (item instanceof Player) {
      final DefaultMutableTreeNode roomNode = (DefaultMutableTreeNode) ((DefaultMutableTreeNode) value).getParent();
      final Object room = roomNode.getUserObject();
      if (room instanceof LockableRoom) {
        if (!ChatServerConnection.DEFAULT_ROOM_NAME.equals(((Room) room).getName()) && ((LockableRoom) room).isOwner(((Player) item).getId())) {
          if (ownerFont == null) {
            nonOwnerFont = this.getFont();
            ownerFont = new Font(nonOwnerFont.getFontName(), nonOwnerFont.getStyle()+Font.BOLD, nonOwnerFont.getSize());
          }
          setFont(ownerFont);
          return l;
        }
      }
    }
    setFont(nonOwnerFont);
    return l;
  }
}
