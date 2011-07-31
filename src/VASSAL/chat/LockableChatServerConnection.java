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
package VASSAL.chat;

public interface LockableChatServerConnection extends ChatServerConnection {

  /** Lock/Unlock the given room */
  void lockRoom(LockableRoom r);

  /** Can a player be invited to this room by me? */
  boolean isInvitable(Player invitee);

  /** Invite a player to this room */
  void sendInvite(Player p);

  /** Process Invitation */
  void doInvite(String playerId, String roomName);

  /** Can a player be kicked from this room by me? */
  boolean isKickable(Player kickee);

  /** Kick a player from this room */
  void doKick (Player p);

  /** Return the name of the default room */
  String getDefaultRoomName();

  /** Is specified room the default room? */
  boolean isDefaultRoom(Room r);
}
