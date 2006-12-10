/*
 * $Id: DefaultRoom.java,v 1.2 2006-12-09 22:49:23 rkinney Exp $
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
import java.util.Iterator;
import java.util.List;

/**
 * A simple Room bean
 */
public class SimpleRoom implements Room {
  private String name;
  private List players = new ArrayList();

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

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setPlayers(Player[] players) {
    this.players.clear();
    for (int i = 0; i < players.length; i++) {
      addPlayer(players[i]);
    }
  }

  public void addPlayer(Player p) {
    int index = players.indexOf(p);
    if (index < 0) {
      players.add(p);
    }
    else {
      players.set(index, p);
    }
  }

  public void removePlayer(Player p) {
    players.remove(p);
  }

  public boolean contains(Player p) {
    return players.contains(p);
  }

  public String toString() {
    return name;
  }

  public int hashCode() {
    return name.hashCode();
  }

  public boolean equals(Object o) {
    if (o instanceof SimpleRoom) {
      return name == null ? false : name.equals(((SimpleRoom) o).name);
    }
    else {
      return false;
    }
  }

  public Iterator getPlayers() {
    return players.iterator();
  }

  public List getPlayerList() {
    return Collections.unmodifiableList(players);
  }
}
