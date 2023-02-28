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

import java.awt.Font;
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

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleRoom;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.SwingUtils;

/**
 * Adds mouse listeners to the RoomTree components: double-click to join a room, etc. Builds a popup when right-clicking
 * on a player or room
 *
 * @author rkinney
 *
 */
public class RoomInteractionControlsInitializer implements ChatControlsInitializer {
  @Deprecated(since = "2022-08-08", forRemoval = true)
  public static final Font POPUP_MENU_FONT = new Font("Dialog", 0, 10); //$NON-NLS-1$
  private final List<PlayerActionFactory> playerActionFactories = new ArrayList<>();
  private final List<RoomActionFactory> roomActionFactories = new ArrayList<>();
  protected ChatServerConnection client;
  private MouseAdapter currentRoomPopupBuilder;
  private MouseAdapter roomPopupBuilder;
  private ActionListener roomCreator;

  public RoomInteractionControlsInitializer(ChatServerConnection client) {
    super();
    this.client = client;
  }

  @Override
  public void initializeControls(final ChatServerControls controls) {
    currentRoomPopupBuilder = new MouseAdapter() {
      @Override
      public void mousePressed(MouseEvent e) {
        maybePopup(e);
      }

      @Override
      public void mouseReleased(MouseEvent e) {
        maybePopup(e);
      }

      private void maybePopup(MouseEvent e) {
        if (e.isPopupTrigger()) {
          final JTree tree = (JTree) e.getSource();
          final TreePath path = tree.getPathForLocation(e.getX(), e.getY());
          if (path != null) {
            final Object target = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
            if (target instanceof Player) {
              final JPopupMenu popup = buildPopupForPlayer((SimplePlayer) target, tree);
              if (popup != null) {
                popup.show(tree, e.getX(), e.getY());
              }
            }
          }
        }
      }
    };
    controls.getCurrentRoom().addMouseListener(currentRoomPopupBuilder);
    roomPopupBuilder = new MouseAdapter() {
      @Override
      public void mousePressed(MouseEvent e) {
        if (e.isPopupTrigger()) {
          maybePopup(e);
        }
      }

      @Override
      public void mouseReleased(MouseEvent e) {
        if (e.isPopupTrigger()) {
          maybePopup(e);
        }
        else if (e.getClickCount() == 2 && SwingUtils.isMainMouseButtonDown(e)) {
          final JTree tree = (JTree) e.getSource();
          final TreePath path = tree.getPathForLocation(e.getX(), e.getY());
          if (path != null) {
            final Object target = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
            if (target instanceof SimpleRoom) {
              final int row = tree.getRowForLocation(e.getX(), e.getY());
              if (tree.isCollapsed(row)) {
                tree.expandRow(row);
              }
              else {
                tree.collapseRow(row);
              }
              doubleClickRoom((Room) target, tree);
            }
          }
        }
      }

      private void maybePopup(MouseEvent e) {
        final JTree tree = (JTree) e.getSource();
        final TreePath path = tree.getPathForLocation(e.getX(), e.getY());
        if (path != null) {
          final Object target = ((DefaultMutableTreeNode) path.getLastPathComponent()).getUserObject();
          JPopupMenu popup = null;

          if (target instanceof Player) {
            popup = buildPopupForPlayer((SimplePlayer) target, tree);
          }
          else if (target instanceof SimpleRoom) {
            popup = buildPopupForRoom((Room) target, tree);
          }

          if (popup != null) {
            popup.show(tree, e.getX(), e.getY());
          }
        }
      }
    };
    controls.getRoomTree().addMouseListener(roomPopupBuilder);
    roomCreator = e -> {
      if (!client.isConnected()) {
        GameModule.getGameModule().warn(Resources.getString("Chat.connect_first"));
        return;
      }
      if ("".equals(controls.getNewRoom().getText())) {
        GameModule.getGameModule().warn(Resources.getString("Chat.name_first"));
        return;
      }
      createRoom(controls.getNewRoom().getText());

      GameModule.getGameModule().warn(Resources.getString("Chat.creating_room", controls.getNewRoom().getText()));
      GameModule.getGameModule().warn(Resources.getString("Chat.explain_created_room"));

      controls.getNewRoom().setText(""); 
    };
    controls.getNewRoom().addActionListener(roomCreator);
    controls.getNewRoomButton().addActionListener(roomCreator);
  }

  protected void createRoom(String name) {
    client.setRoom(new SimpleRoom(name));
  }

  public JPopupMenu buildPopupForRoom(Room room, JTree tree) {
    final JPopupMenu popup = new JPopupMenu();
    for (final RoomActionFactory f : roomActionFactories) {
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
    final JPopupMenu popup = new JPopupMenu();
    for (final PlayerActionFactory f : playerActionFactories) {
      final Action a = f.getAction(target, tree);
      if (a != null) {
        popup.add(a);
      }
    }
    return popup.getComponentCount() == 0 ? null : popup;
  }

  @Override
  public void uninitializeControls(ChatServerControls controls) {
    controls.getRoomTree().removeMouseListener(roomPopupBuilder);
    controls.getCurrentRoom().removeMouseListener(currentRoomPopupBuilder);
    controls.getNewRoom().removeActionListener(roomCreator);
    controls.getNewRoomButton().removeActionListener(roomCreator);
  }
}
