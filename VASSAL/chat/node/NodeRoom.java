/*
 * $Id: NodeRoom.java,v 1.7 2006-12-12 06:39:41 rkinney Exp $
 *
 * Copyright (c) 2004 by Rodney Kinney
 *
 */
package VASSAL.chat.node;

import java.util.List;
import java.util.Properties;
import VASSAL.chat.LockableRoom;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.Player;
import VASSAL.configure.StringArrayConfigurer;

/**
 * Subclass of {@link SimpleRoom} for use on the client side of a hierarchical server
 */
public class NodeRoom extends SimpleRoom implements LockableRoom {
  public static final String OWNER = "owner";
  public static final String MEMBERS = "members";
  public static final String STATUS = "status";
  private static final String LOCKED = "locked";

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
    locked = "true".equals(p.getProperty(LOCKED));
  }

  public Properties getInfo() {
    Properties p = new Properties();
    if (owner != null) {
      p.setProperty(OWNER, owner);
    }
    if (locked) {
      p.setProperty(LOCKED,"true");
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
    List l = getPlayerList();
    NodePlayer[] p = (NodePlayer[]) l.toArray(new NodePlayer[l.size()]);
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

  public String getOwner() {
    return owner;
  }

  public void lock() {
    locked = true;
  }

  public boolean isLocked() {
    return locked;
  }
}
