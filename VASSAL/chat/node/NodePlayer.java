/*
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: May 11, 2003
 */
package VASSAL.chat.node;

import java.util.Properties;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleStatus;

/**
 * A {@link SimplePlayer} subclass used in clients of the hierarchical server
 */
public class NodePlayer extends SimplePlayer {
  public static final String ID = "id";
  private String id;

  public NodePlayer(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof NodePlayer)) return false;

    final NodePlayer hPlayer = (NodePlayer) o;

    if (id != null ? !id.equals(hPlayer.id) : hPlayer.id != null) return false;

    return true;
  }

  public int hashCode() {
    return (id != null ? id.hashCode() : 0);
  }

  public static final String NAME = "name";
  public static final String LOOKING = "looking";
  public static final String AWAY = "away";
  public static final String PROFILE = "profile";

  public void setInfo(Properties p) {
    name = p.getProperty(NAME,"???");
    id = p.getProperty(ID,id);
    setStatus(new SimpleStatus("true".equals(p.getProperty(LOOKING)),"true".equals(p.getProperty(AWAY)),p.getProperty(PROFILE,"")));
  }



  public Properties toProperties() {
    Properties p1 = new Properties();
    if (name != null) {
      p1.put(NAME,name);
    }
    SimpleStatus status = (SimpleStatus)getStatus();
    p1.put(LOOKING,""+status.isLooking());
    p1.put(AWAY,""+status.isAway());
    String profile = status.getProfile();
    if (profile != null) {
      p1.put(PROFILE,profile);
    }
    Properties p = p1;
    p.put(ID,id);
    return p;
  }

}
