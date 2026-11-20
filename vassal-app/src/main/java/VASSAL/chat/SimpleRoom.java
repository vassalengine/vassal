/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A simple Room bean.
 *
 * <p>Instances are shared between threads: a chat client's network thread adds and removes
 * players while the event dispatch thread paints the room list, so every method which touches
 * the player list is synchronized and {@link #getPlayerList()} hands back a snapshot rather
 * than a live view. A live view could, and did, change size between a caller's size check and
 * its next read. See vassalengine/vassal#15107.</p>
 */
public class SimpleRoom implements Room {
  private String name;
  private final List<Player> players = new ArrayList<>();

  public SimpleRoom() {
  }

  public SimpleRoom(String name) {
    this(name, new Player[0]);
  }

  public SimpleRoom(String name, Player[] players) {
    this.name = name;
    setPlayers(players);
  }

  public SimpleRoom(Room copy) {
    this.name = copy.getName();
    players.addAll(copy.getPlayerList());
  }

  @Override
  public synchronized String getName() {
    return name;
  }

  public synchronized void setName(String name) {
    this.name = name;
  }

  public synchronized void setPlayers(Player[] players) {
    this.players.clear();
    for (final Player p : players) {
      addPlayer(p);
    }
  }

  @Override
  public synchronized void addPlayer(Player p) {
    final int index = players.indexOf(p);
    if (index < 0) {
      players.add(p);
    }
    else {
      players.set(index, p);
    }
  }

  public synchronized Player getPlayer(String id) {
    for (final Player player : players) {
      if (player.getId().equals(id)) {
        return player;
      }
    }
    return null;
  }

  @Override
  public synchronized void removePlayer(Player p) {
    players.remove(p);
  }

  public synchronized boolean contains(Player p) {
    return players.contains(p);
  }

  @Override
  public synchronized String toString() {
    return name;
  }

  public synchronized int numPlayers() {
    return players.size();
  }

  @Override
  public synchronized int hashCode() {
    return name.hashCode();
  }

  @Override
  public boolean equals(Object o) {
    if (o instanceof SimpleRoom) {
      final String n = getName();
      return n != null && n.equals(((SimpleRoom) o).getName());
    }
    else {
      return false;
    }
  }

  /**
   * {@inheritDoc}
   *
   * <p>The returned list is an unmodifiable <i>snapshot</i>: it does not change when players
   * subsequently join or leave, so a caller may check its size and then index it without the
   * room emptying underneath.</p>
   */
  @Override
  public synchronized List<Player> getPlayerList() {
    return Collections.unmodifiableList(new ArrayList<>(players));
  }
}
