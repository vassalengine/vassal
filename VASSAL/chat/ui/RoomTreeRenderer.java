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
/*
 * $Id: RoomTreeRenderer.java,v 1.4 2006-12-10 06:34:54 rkinney Exp $
 *
 * Copyright (c) 2004 by Rodney Kinney
 *
 */
package VASSAL.chat.ui;

import java.util.List;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleStatus;

/** Cell render component for {@link RoomTree} */
public class RoomTreeRenderer extends DefaultTreeCellRenderer {
  private Icon away;
  private Icon looking;

  public RoomTreeRenderer() {
    java.net.URL image = getClass().getResource("/images/playerAway.gif");
    if (image != null) {
      away = new ImageIcon(image);
    }

    image = getClass().getResource("/images/playerLooking.gif");
    if (image != null) {
      looking = new ImageIcon(image);
    }
  }

  public java.awt.Component getTreeCellRendererComponent(JTree tree, Object value,
                                                         boolean sel,
                                                         boolean expanded,
                                                         boolean leaf, int row,
                                                         boolean hasFocus) {
    super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
    value = ((DefaultMutableTreeNode) value).getUserObject();
    if (value instanceof Player) {
      if (((SimpleStatus)((Player) value).getStatus()).isAway()) {
        setIcon(away);
      }
      else if (((SimpleStatus)((SimplePlayer) value).getStatus()).isLooking()) {
        setIcon(looking);
      }
      else {
        setIcon(null);
      }
    }
    else if (value instanceof SimpleRoom) {
      List players = ((Room) value).getPlayerList();
      if (!expanded || players.size() == 0) {
        setText(getText() + " [" + players.size() + "]");
      }
    }
    return this;
  }
}
