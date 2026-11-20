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
package VASSAL.chat;

import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The player list a room hands out has to be a snapshot. It used to be a live view of the
 * room's own collection, so a caller which checked its size and then indexed it could be
 * overtaken by the chat client's network thread removing a player in between.
 * See vassalengine/vassal#15107.
 */
public class SimpleRoomTest {

  private static SimplePlayer player(String id) {
    final SimplePlayer p = new SimplePlayer(id);
    p.setId(id);
    return p;
  }

  @Test
  public void testPlayerListDoesNotShrinkWhenAPlayerLeaves() {
    final SimplePlayer alice = player("alice");
    final SimplePlayer bob = player("bob");
    final SimpleRoom room = new SimpleRoom("main", new Player[] {alice, bob});

    final List<Player> snapshot = room.getPlayerList();
    assertEquals(2, snapshot.size());

    room.removePlayer(bob);

    // The caller is still holding the list it was given, and may still index all of it.
    assertEquals(2, snapshot.size(), "the list handed out must not track later departures");
    assertEquals(bob, snapshot.get(1));
    assertEquals(1, room.getPlayerList().size(), "a fresh call sees the departure");
  }

  @Test
  public void testPlayerListDoesNotGrowWhenAPlayerJoins() {
    final SimpleRoom room = new SimpleRoom("main", new Player[] {player("alice")});

    final List<Player> snapshot = room.getPlayerList();
    room.addPlayer(player("bob"));

    assertEquals(1, snapshot.size(), "the list handed out must not track later arrivals");
    assertEquals(2, room.getPlayerList().size());
  }

  @Test
  public void testPlayerListIsStillUnmodifiable() {
    final SimpleRoom room = new SimpleRoom("main", new Player[] {player("alice")});
    final List<Player> snapshot = room.getPlayerList();
    assertThrows(UnsupportedOperationException.class, () -> snapshot.add(player("mallory")));
  }

  /**
   * A room is handed between a network thread and the event dispatch thread, so every read of
   * it has to be safe while the other thread writes. Before the fix this reliably threw.
   */
  @Test
  public void testConcurrentJoinsAndDeparturesDoNotBreakAReader() throws Exception {
    final SimpleRoom room = new SimpleRoom("main");
    final SimplePlayer[] roster = new SimplePlayer[8];
    for (int i = 0; i < roster.length; ++i) {
      roster[i] = player("p" + i);
    }

    final AtomicReference<Throwable> failure = new AtomicReference<>();

    final Thread writer = new Thread(() -> {
      for (int n = 0; n < 20000; ++n) {
        final SimplePlayer p = roster[n % roster.length];
        if ((n & 1) == 0) {
          room.addPlayer(p);
        }
        else {
          room.removePlayer(p);
        }
      }
    });

    final Thread reader = new Thread(() -> {
      try {
        for (int n = 0; n < 20000; ++n) {
          // Exactly what RoomTree.setRooms and RoomTreeRenderer do: size, then index.
          final List<Player> players = room.getPlayerList();
          for (int i = 0; i < players.size(); ++i) {
            assertTrue(players.get(i) != null);
          }
        }
      }
      catch (Throwable t) {
        failure.set(t);
      }
    });

    writer.start();
    reader.start();
    writer.join();
    reader.join();

    assertNull(failure.get(), () -> "reading a room while players came and went threw " + failure.get());
  }
}
