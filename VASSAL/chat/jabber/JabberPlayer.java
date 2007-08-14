/*
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
package VASSAL.chat.jabber;

import java.util.HashMap;
import java.util.Map;

import org.jivesoftware.smack.util.StringUtils;

import VASSAL.chat.SimplePlayer;

public class JabberPlayer extends SimplePlayer {
  private String jid;
  private JabberRoom joinedRoom;

  private JabberPlayer(String name, String jid) {
    super(name);
    this.jid = jid;
  }

  public String getJid() {
    return jid;
  }
  
  public boolean equals(Object o) {
    return o instanceof JabberPlayer && jid.equals(((JabberPlayer)o).jid);
  }
  
  public int hashCode() {
    return jid.hashCode();
  }
  
  public String toString() {
    return name+" ("+jid+")";
  }
  
  public void join(JabberRoom room) {
    if (joinedRoom != null) {
      joinedRoom.removePlayer(this);
      if (joinedRoom.getPlayerList().size() == 0) {
        JabberRoom.deleteRoom(joinedRoom.getJID());
      }
    }
    room.addPlayer(this);
    joinedRoom = room;
  }
  
  private static Map<String, JabberPlayer> jidToPlayer = new HashMap<String, JabberPlayer>();

  public static JabberPlayer getPlayer(String jid) {
    if (jid == null) {
      return null;
    }
    JabberPlayer p = jidToPlayer.get(jid);
    if (p == null) {
      p = new JabberPlayer(StringUtils.parseResource(jid), jid);
      jidToPlayer.put(jid, p);
    }
    return p;
  }
  
  public static JabberPlayer getPlayerByName(JabberClient client, String name) {
    return getPlayer(StringUtils.escapeNode(name) + "@" + client.getHost() + "/VASSAL");
  }

  public JabberRoom getJoinedRoom() {
    return joinedRoom;
  }

  public static void deletePlayer(String jid) {
    jidToPlayer.remove(jid);
  }
}
