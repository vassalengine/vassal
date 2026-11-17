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

import java.util.ArrayList;
import java.util.List;

import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.i18n.Resources;

/**
 * JTree component displaying chat rooms on the server
 * @author rkinney
 *
 */
public class RoomTree extends JTree {
  private static final long serialVersionUID = 1L;

  protected DefaultTreeModel model;
  protected DefaultMutableTreeNode root;

  public RoomTree() {
    setRootVisible(false);
    setShowsRootHandles(true);
    root = new DefaultMutableTreeNode(Resources.getString("Chat.server")); //$NON-NLS-1$
    model = new DefaultTreeModel(root) {
      private static final long serialVersionUID = 1L;

      @Override
      public boolean isLeaf(Object node) {
        return ((DefaultMutableTreeNode) node).getUserObject() instanceof Player;
      }
    };
    setModel(model);
    setCellRenderer(new RoomTreeRenderer());
  }

  public synchronized void setRooms(Room[] room) {
    if (room == null) {
      room = new Room[0];
    }

// Remove rooms no longer present
    for (int i = 0; i < root.getChildCount(); ++i) {
      final Room r = roomAt(i);
      int j = 0;
      for (j = 0; j < room.length; ++j) {
        if (room[j] == r) {
          break;
        }
        else if (room[j].equals(r)) {
          model.valueForPathChanged(
            new TreePath(((DefaultMutableTreeNode) root.getChildAt(i))
              .getPath()), room[j]);
          break;
        }
      }
      if (j >= room.length) { // No match
        model.removeNodeFromParent((DefaultMutableTreeNode) root.getChildAt(i--));
      }
    }
// Add new rooms
    for (int i = 0; i < room.length; ++i) {
      int j;
      for (j = 0; j < root.getChildCount(); ++j) {
        if (roomAt(j) == room[i]) {
          break;
        }
      }
      if (j >= root.getChildCount()) {
        model.insertNodeInto(new DefaultMutableTreeNode(room[i]), root, i);
      }
    }
// Update players
    for (int i = 0; i < root.getChildCount(); ++i) {
      final DefaultMutableTreeNode node = (DefaultMutableTreeNode) root.getChildAt(i);
      // Copied, not held: a Room's player list may be a live view of a collection which the
      // chat client's network thread is still adding to and removing from, in which case its
      // size can change between the loops below which match the node count to it and the loop
      // after them which indexes it. See vassalengine/vassal#15107.
      final List<Player> p = new ArrayList<>(roomAt(i).getPlayerList());
      while (p.size() < node.getChildCount()) {
        model.removeNodeFromParent((DefaultMutableTreeNode) node.getChildAt(0));
      }
      while (p.size() > node.getChildCount()) {
        // A placeholder: every node's user object is set from p immediately below. It must not
        // be room[i], which is a Room standing in a player's position, and is out of bounds
        // altogether should the tree ever hold more rooms than were passed in.
        model.insertNodeInto(new DefaultMutableTreeNode(), node, 0);
      }
      for (int j = 0; j < node.getChildCount(); ++j) {
        model.valueForPathChanged(
          new TreePath(((DefaultMutableTreeNode) node.getChildAt(j)).getPath()), p.get(j));
      }
    }
  }

  protected Room roomAt(int index) {
    return (Room) ((DefaultMutableTreeNode) root.getChildAt(index)).getUserObject();
  }
}
