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

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.node.NodeClient;
import VASSAL.tools.version.VersionUtils;
import java.awt.Component;
import java.net.URL;
import java.util.List;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

import VASSAL.chat.Player;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.SimpleStatus;

/** Cell render component for {@link RoomTree} */
public class RoomTreeRenderer extends DefaultTreeCellRenderer {
  private static final long serialVersionUID = 1L;

  private Icon away;
  private Icon looking;

  public RoomTreeRenderer() {
    URL image = getClass().getResource("/images/playerAway.gif"); //$NON-NLS-1$
    if (image != null) {
      away = new ImageIcon(image);
    }

    image = getClass().getResource("/images/playerLooking.gif"); //$NON-NLS-1$
    if (image != null) {
      looking = new ImageIcon(image);
    }
  }

  @Override
  public Component getTreeCellRendererComponent(
    JTree tree,
    Object value,
    boolean sel,
    boolean expanded,
    boolean leaf,
    int row,
    boolean hasFocus) {

    super.getTreeCellRendererComponent(
      tree, value, sel, expanded, leaf, row, hasFocus
    );

    putClientProperty("html.disable", Boolean.TRUE); //$NON-NLS-1$

    final Object item = ((DefaultMutableTreeNode) value).getUserObject();
    if (item instanceof Player) {
      if (((SimpleStatus)((Player) item).getStatus()).isAway()) {
        setIcon(away);
      }
      else if (((SimpleStatus)((SimplePlayer) item).getStatus()).isLooking()) {
        setIcon(looking);
      }
      else {
        setIcon(null);
      }

    }
    else if (item instanceof SimpleRoom) {
      String version = null;
      final SimpleRoom room = (SimpleRoom) item;

      if (!room.getName().equals(ChatServerConnection.DEFAULT_ROOM_NAME)) {
        if (room.getPlayerList().get(0).getStatus() instanceof SimpleStatus) {
          final SimpleStatus status = (SimpleStatus) room.getPlayerList().get(0).getStatus();
          version = VersionUtils.truncateToIncrementalVersion(status.getClient()) + '/' + NodeClient.cleanVersion(status.getModuleVersion());
        }
      }
      final List<Player> players = room.getPlayerList();
      this.putClientProperty("html.disable", Boolean.FALSE);
      this.setText(
        "<html>" + getText() + " (" + players.size() + ")" +
        (version == null ? "" : "<span style=\"color:#A0A0A0;\"> - [" + version + "]</span>") + "</html>"
      );
    }
    return this;
  }
}
