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
package VASSAL.chat.jabber;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.jivesoftware.smack.util.StringUtils;

import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleStatus;

public class JabberPlayer extends SimplePlayer {
  private String jid;
  private JabberRoom joinedRoom;

  private JabberPlayer(String jid) {
    super(jid,"???",new SimpleStatus()); //$NON-NLS-1$
    this.jid = jid;
  }

  public String getJid() {
    return jid;
  }

  public boolean equals(Object o) {
    return o instanceof JabberPlayer && jid.equals(((JabberPlayer) o).jid);
  }

  public int hashCode() {
    return jid.hashCode();
  }

  public String toString() {
    return name + " (" + StringUtils.parseName(jid) + ")"; //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void join(JabberRoom room) {
    if (joinedRoom != null) {
      joinedRoom.removePlayer(this);
    }
    room.addPlayer(this);
    joinedRoom = room;
  }

  public JabberRoom getJoinedRoom() {
    return joinedRoom;
  }

  public String getRawJid() {
    if (jid.contains("/")) { //$NON-NLS-1$
      return StringUtils.parseName(jid) + "@" + StringUtils.parseServer(jid); //$NON-NLS-1$
    }
    return jid;
  }

  public String getLoginName() {
    return StringUtils.parseName(jid);
  }

  public static String xmppAddressToJid(String participant) {

    final String address = StringUtils.parseServer(participant);
    final String[] parts = address.split("\\."); //$NON-NLS-1$
    final String server = parts[parts.length-1];
    final String nick = StringUtils.parseResource(participant);

    return nick+"@"+server+JabberClient.JID_RESOURCE; //$NON-NLS-1$
  }

  public static class Manager {
    private Map<String, JabberPlayer> jidToPlayer = new HashMap<String, JabberPlayer>();

    public JabberPlayer getPlayer(String jid) {
      if (jid == null) {
        return null;
      }
      JabberPlayer p = jidToPlayer.get(jid);
      if (p == null) {
        p = new JabberPlayer(jid);
        jidToPlayer.put(jid, p);
      }
      return p;
    }

    public JabberPlayer getPlayerByLogin(JabberClient client, String login) {
      return getPlayer(login + "@" + client.getHost() + JabberClient.JID_RESOURCE); //$NON-NLS-1$
    }

    public synchronized void deletePlayer(String jid) {
      jidToPlayer.remove(jid);
    }

    public synchronized void clear() {
      jidToPlayer.clear();
    }

    public Collection<JabberPlayer> getAllPlayers() {
      return jidToPlayer.values();
    }
  }
}
