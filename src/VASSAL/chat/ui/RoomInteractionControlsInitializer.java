/*
 * $Id$
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

import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.Action;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleRoom;

/**
 * Adds mouse listeners to the RoomTree components: double-click to join a room, etc. Builds a popup when right-clicking
 * on a player or room
 *
 * @author rkinney
 *
 */
public class RoomInteractionControlsInitializer implements ChatControlsInitializer {
  public static final Font POPUP_MENU_FONT = new Font("Dialog", 0, 10); //$NON-NLS-1$
  private List<PlayerActionFactory> playerActionFactories =
    new ArrayList<PlayerActionFactory>();
  private List<RoomActionFactory> roomActionFactories =
    new ArrayList<RoomActionFactory>();
  protected ChatServerConnection client;
  private MouseAdapter currentRoomPopupBuilder;
  private MouseAdapter roomPopupBuilder;
  private ActionListener roomCreator;

  public RoomInteractionControlsInitializer(ChatServerConnection client) {
    super();
    this.client = client;
  }

  public void initializeControls(final ChatServerControls controls) {
    currentRoomPopupBuilder = new MouseAdapter() {
      public void mouseReleased(MouseEvent evt) {
        JTree tree = (JTree) evt.getSource();
        if (evt.isMetaDown()) {
          TreePath path = tree.getPathForLocation(evt.getX(), evt.getY());
          if (path != null) {
            Object target = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
            if (target instanceof Player) {
              JPopupMenu popup = buildPopupForPlayer((SimplePlayer) target, tree);
              if (popup != null) {
                for (int i = 0, n = popup.getComponentCount(); i < n; ++i) {
                  popup.getComponent(i).setFont(POPUP_MENU_FONT);
                }
                popup.show(tree, evt.getX(), evt.getY());
              }
            }
          }
        }
      }
    };
    controls.getCurrentRoom().addMouseListener(currentRoomPopupBuilder);
    roomPopupBuilder = new MouseAdapter() {
      public void mouseReleased(MouseEvent evt) {
        JTree tree = (JTree) evt.getSource();
        TreePath path = tree.getPathForLocation(evt.getX(), evt.getY());
        if (path != null) {
          Object target = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
          if (target instanceof Player) {
            if (evt.isMetaDown()) {
              JPopupMenu popup = buildPopupForPlayer((SimplePlayer) target, tree);
              for (int i = 0, n = popup.getComponentCount(); i < n; ++i) {
                popup.getComponent(i).setFont(POPUP_MENU_FONT);
              }
              popup.show(tree, evt.getX(), evt.getY());
            }
          }
          else if (target instanceof SimpleRoom) {
            if (evt.isMetaDown()) {
              JPopupMenu popup = buildPopupForRoom((VASSAL.chat.Room) target, tree);
              if (popup != null) {
                for (int i = 0, n = popup.getComponentCount(); i < n; ++i) {
                  popup.getComponent(i).setFont(POPUP_MENU_FONT);
                }
                popup.show(tree, evt.getX(), evt.getY());
              }
            }
            else if (evt.getClickCount() == 2) {
              int row = tree.getRowForLocation(evt.getX(), evt.getY());
              if (tree.isCollapsed(row)) {
                tree.expandRow(row);
              }
              else {
                tree.collapseRow(row);
              }
              doubleClickRoom((VASSAL.chat.Room) target, tree);
            }
          }
        }
      }
    };
    controls.getRoomTree().addMouseListener(roomPopupBuilder);
    roomCreator = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        createRoom(controls.getNewRoom().getText());
        controls.getNewRoom().setText(""); //$NON-NLS-1$
      }
    };
    controls.getNewRoom().addActionListener(roomCreator);
  }

  protected void createRoom(String name) {
    client.setRoom(new SimpleRoom(name));
  }

  public JPopupMenu buildPopupForRoom(Room room, JTree tree) {
    JPopupMenu popup = new JPopupMenu();
    for (RoomActionFactory f : roomActionFactories) {
      popup.add(f.getAction(room, tree));
    }
    return popup.getComponentCount() == 0 ? null : popup;
  }

  public void doubleClickRoom(Room room, JTree tree) {
    if (!room.equals(client.getRoom())) {
      new JoinRoomAction(room, client).actionPerformed(null);
    }
  }

  public void addPlayerActionFactory(PlayerActionFactory f) {
    playerActionFactories.add(f);
  }

  public void addRoomActionFactory(RoomActionFactory f) {
    roomActionFactories.add(f);
  }

  public JPopupMenu buildPopupForPlayer(SimplePlayer target, JTree tree) {
    JPopupMenu popup = new JPopupMenu();
    for (PlayerActionFactory f : playerActionFactories) {
      final Action a = f.getAction(target, tree);
      if (a != null) {
        popup.add(a);
      }
    }
    return popup.getComponentCount() == 0 ? null : popup;
  }

  public void uninitializeControls(ChatServerControls controls) {
    controls.getRoomTree().removeMouseListener(roomPopupBuilder);
    controls.getCurrentRoom().removeMouseListener(currentRoomPopupBuilder);
    controls.getNewRoom().removeActionListener(roomCreator);
  }
}
