/*
 * $Id$
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
package VASSAL.chat.node;

import java.util.List;
import java.util.Properties;

import VASSAL.chat.LockableRoom;
import VASSAL.chat.Player;
import VASSAL.chat.SimpleRoom;
import VASSAL.configure.StringArrayConfigurer;

/**
 * Subclass of {@link SimpleRoom} for use on the client side of a hierarchical server
 */
public class NodeRoom extends SimpleRoom implements LockableRoom {
  public static final String OWNER = "owner"; //$NON-NLS-1$
  public static final String MEMBERS = "members"; //$NON-NLS-1$
  public static final String STATUS = "status"; //$NON-NLS-1$
  public static final String LOCKED = "locked"; //$NON-NLS-1$

  private String owner; // The userId (as in GameModule.getUserId() of the player who created the room
  private String[] members; // The players who were in the room at the time it was closed;
  private boolean locked; // If locked, no new players may join

  public NodeRoom() {
  }

  public void setOwner(String owner) {
    this.owner = owner;
  }

  public NodeRoom(String name) {
    super(name);
  }

  public NodeRoom(String name, Player[] players) {
    super(name, players);
  }

  public void setInfo(Properties p) {
    owner = p.getProperty(OWNER,owner);
    members = StringArrayConfigurer.stringToArray(p.getProperty(MEMBERS,StringArrayConfigurer.arrayToString(members)));
    locked = "true".equals(p.getProperty(LOCKED)); //$NON-NLS-1$
  }

  public Properties getInfo() {
    Properties p = new Properties();
    if (owner != null) {
      p.setProperty(OWNER, owner);
    }
    if (locked) {
      p.setProperty(LOCKED,"true"); //$NON-NLS-1$
    }
    if (members != null) {
      p.setProperty(MEMBERS, StringArrayConfigurer.arrayToString(members));
    }
    return p;
  }

  /**
   * Set the members to be the list of players currently in the room
   */
  public void setMembersToCurrentPlayers() {
    List<Player> l = getPlayerList();
    NodePlayer[] p = l.toArray(new NodePlayer[l.size()]);
    members = new String[p.length];
    for (int i = 0; i < p.length; i++) {
      members[i] = p[i].getId();
    }
  }

  public String[] getMembers() {
    return members;
  }

  public boolean isMember(NodePlayer p) {
    boolean isMember = false;
    if (members != null) {
      for (int i = 0; !isMember && i < members.length; i++) {
        isMember = p.getId().equals(members[i]);
      }
    }
    return isMember;
  }

  public boolean isOwner(NodePlayer p) {
    return p != null && owner != null && owner.equals(p.getId());
  }

  public boolean isOwner(String jid) {
    return jid.equals(owner);
  }

  public String getOwner() {
    return owner;
  }

  public Player getOwningPlayer() {
    return getPlayer(owner);
  }

  public void lock() {
    locked = true;
  }

  public void toggleLock() {
    locked = !locked;
  }

  public boolean isLocked() {
    return locked;
  }
}
