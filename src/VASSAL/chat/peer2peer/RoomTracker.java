/*
 * $Id$
 *
 * Copyright (c) 2007 by Rodney Kinney
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
package VASSAL.chat.peer2peer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import VASSAL.chat.Player;
import VASSAL.chat.Room;

/**
 * Tracks players entering/exiting a room
 */
public class RoomTracker {
  private List<Player> joinedPlayers;
  private List<Player> leftPlayers;

  public RoomTracker() {
    joinedPlayers = new ArrayList<Player>();
    leftPlayers = new ArrayList<Player>();
  }

  public void init(Room r) {
    joinedPlayers.clear();
    leftPlayers.clear();
    leftPlayers.addAll(r.getPlayerList());
  }

  public void finalize(Room r) {
    for (Player p : r.getPlayerList()) {
      if (!leftPlayers.contains(p)) {
        joinedPlayers.add(p);
      }
      leftPlayers.remove(p);
    }
  }

  public Enumeration<Player> getJoinedPlayers() {
    return Collections.enumeration(joinedPlayers);
  }

  public Enumeration<Player> getLeftPlayers() {
    return Collections.enumeration(leftPlayers);
  }
}
