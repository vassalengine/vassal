/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;

import VASSAL.chat.LockableRoom;

/**
 * Renders rooms with a "locked" icon if locked
 */
public class LockableRoomTreeRenderer extends RoomTreeRenderer {
  private static final long serialVersionUID = 1L; 

  private Icon lockedIcon;
  public LockableRoomTreeRenderer() {
    java.net.URL image = getClass().getResource("/images/lockedRoom.gif"); //$NON-NLS-1$
    if (image != null) {
      lockedIcon = new ImageIcon(image);
    }
  }

  public Component getTreeCellRendererComponent(JTree tree, Object value,
                                                boolean sel,
                                                boolean expanded,
                                                boolean leaf, int row,
                                                boolean hasFocus) {
    JLabel l =  (JLabel) super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
    value = ((DefaultMutableTreeNode) value).getUserObject();
    if (lockedIcon != null
      && value instanceof LockableRoom
      && ((LockableRoom)value).isLocked()) {
     l.setIcon(lockedIcon);
    }
    return l;
  }
}
