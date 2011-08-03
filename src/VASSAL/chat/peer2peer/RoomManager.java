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
import java.util.List;

import org.litesoft.p2pchat.PeerInfo;

import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.SimpleRoom;
import VASSAL.i18n.Resources;

public class RoomManager {
  private List<Room> rooms = new ArrayList<Room>();
  private SimpleRoom defaultRoom =
    new SimpleRoom(Resources.getString("Chat.main_room")); //$NON-NLS-1$

  public RoomManager() {
    rooms.add(defaultRoom);
  }

  public Room[] update(PeerInfo info) {
    P2PPlayer p = new P2PPlayer(info);
    Room oldRoom = getRoomContaining(p);
    Room newRoom = new SimpleRoom(p.getRoom());
    if (rooms.contains(newRoom)) {
      newRoom = rooms.get(rooms.indexOf(newRoom));
    }
    else {
      rooms.add(newRoom);
    }
    newRoom.addPlayer(p);
    if (oldRoom != null && !oldRoom.equals(newRoom)) {
      oldRoom.removePlayer(p);
      if (oldRoom.getPlayerList().size() == 0 &&
          !oldRoom.equals(defaultRoom)) {
        rooms.remove(oldRoom);
      }
    }
    return getRooms();
  }

  public Room[] remove(PeerInfo info) {
    P2PPlayer p = new P2PPlayer(info);
    for (int i = 0; i < rooms.size(); ++i) {
      Room r = rooms.get(i);
      r.removePlayer(p);
      if (r.getPlayerList().size() == 0 &&
          !r.equals(defaultRoom)) {
        rooms.remove(i--);
      }
    }
    return getRooms();
  }

  public P2PPlayer getPlayerById(String id) {
    for (Room r : rooms) {
      for (Player p : r.getPlayerList()) {
        P2PPlayer p2pp = (P2PPlayer) p;
        if (id.equals(p2pp.getId())) {
          return p2pp;
        }
      }
    }
    return null;
  }

  public Room[] getRooms() {
    return rooms.toArray(new Room[rooms.size()]);
/*
//  System.err.println("--------");
    Room[] r = new Room[rooms.size()];
    for (int i = 0; i < r.length; ++i) {
      r[i] = (VASSAL.chat.Room) rooms.elementAt(i);
      //      System.err.println("Room "+r[i]);
      //      for (int j=0;j<r[i].players.length;++j) {
      //    System.err.println("  "+((P2PPlayer)r[i].players[j]).summary());
      //      }
    }
    return r;
*/
  }

  public SimpleRoom getRoomContaining(Player p) {
    for (Room r : rooms) {
      SimpleRoom sr = (SimpleRoom) r;
      if (sr.contains(p)) {
        return sr;
      }
    }
    return null;
  }

  public void setDefaultRoomName(String name) {
    defaultRoom.setName(name);
  }

  public VASSAL.chat.Room getDefaultRoom() {
    return defaultRoom;
  }

  public void clear() {
    rooms.clear();
    defaultRoom.setPlayers(new Player[0]);
    rooms.add(defaultRoom);
  }
}
