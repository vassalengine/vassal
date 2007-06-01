/*
 * $Id$
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
package VASSAL.chat;

import java.util.List;

/**
 * Represents a game room on the server
 * @author rkinney
 *
 */
public interface Room {
  /** The display name of this room */
  String getName();
  /** Return an (unmodifiable) list of players */
  List<Player> getPlayerList();
  /** Add a player to this room */
  void addPlayer(Player p);
  /** Remove a player from this room */
  void removePlayer(Player p);
}
