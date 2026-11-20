/*
 *
 * Copyright (c) 2026 by the VASSAL Development Team
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
import java.util.Collections;
import java.util.List;

import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;

import org.junit.jupiter.api.Test;

import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.SimplePlayer;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * {@link RoomTree#setRooms} matches the number of player nodes to the size of the room's
 * player list and then indexes that list. If the list is a live view of a collection the chat
 * client's network thread is still editing, a player leaving in between makes the index run
 * off the end -- which is what vassalengine/vassal#15107 reported, from a peer-to-peer
 * session in which peers were connecting and disconnecting throughout.
 */
public class RoomTreeTest {

  /** A Room whose player list is a live view, as SimpleRoom's used to be. */
  private static class LiveRoom implements Room {
    private final String name;
    private final List<Player> players = new ArrayList<>();

    LiveRoom(String name, Player... initial) {
      this.name = name;
      Collections.addAll(players, initial);
    }

    @Override
    public String getName() {
      return name;
    }

    @Override
    public List<Player> getPlayerList() {
      return Collections.unmodifiableList(players);
    }

    @Override
    public void addPlayer(Player p) {
      players.add(p);
    }

    @Override
    public void removePlayer(Player p) {
      players.remove(p);
    }

    @Override
    public String toString() {
      return name;
    }
  }

  private static SimplePlayer player(String id) {
    final SimplePlayer p = new SimplePlayer(id);
    p.setId(id);
    return p;
  }

  @Test
  public void testSurvivesAPlayerLeavingWhileTheTreeIsBeingUpdated() {
    final SimplePlayer alice = player("alice");
    final SimplePlayer bob = player("bob");
    final LiveRoom room = new LiveRoom("main", alice, bob);

    final RoomTree tree = new RoomTree();
    tree.setRooms(new Room[] {room});
    assertEquals(2, tree.getModel().getChildCount(tree.getModel().getChild(tree.getModel().getRoot(), 0)));

    // Drop a player at the moment the tree writes the first one back, which is the window the
    // network thread hit in the field: the node count has already been matched to the list,
    // and the list is about to be indexed.
    final boolean[] fired = {false};
    tree.getModel().addTreeModelListener(new TreeModelListener() {
      @Override
      public void treeNodesChanged(TreeModelEvent e) {
        if (!fired[0]) {
          fired[0] = true;
          room.removePlayer(bob);
        }
      }

      @Override
      public void treeNodesInserted(TreeModelEvent e) {
      }

      @Override
      public void treeNodesRemoved(TreeModelEvent e) {
      }

      @Override
      public void treeStructureChanged(TreeModelEvent e) {
      }
    });

    assertDoesNotThrow(() -> tree.setRooms(new Room[] {room}),
      "a player leaving mid-update must not throw out of the tree update");
  }

  @Test
  public void testTracksPlayersJoiningAndLeavingBetweenUpdates() {
    final SimplePlayer alice = player("alice");
    final SimplePlayer bob = player("bob");
    final LiveRoom room = new LiveRoom("main", alice);

    final RoomTree tree = new RoomTree();
    tree.setRooms(new Room[] {room});
    assertEquals(1, playerCount(tree));

    room.addPlayer(bob);
    tree.setRooms(new Room[] {room});
    assertEquals(2, playerCount(tree));

    room.removePlayer(alice);
    tree.setRooms(new Room[] {room});
    assertEquals(1, playerCount(tree));
  }

  private static int playerCount(RoomTree tree) {
    return tree.getModel().getChildCount(tree.getModel().getChild(tree.getModel().getRoot(), 0));
  }
}
