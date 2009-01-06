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
  public static final String ID = "id"; //$NON-NLS-1$

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

  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String LOOKING = "looking"; //$NON-NLS-1$
  public static final String AWAY = "away"; //$NON-NLS-1$
  public static final String PROFILE = "profile"; //$NON-NLS-1$
  public static final String CLIENT = "client"; //$NON-NLS-1$
  public static final String IP = "ip"; //$NON-NLS-1$
  public static final String MODULE_VERSION = "moduleVersion"; //$NON-NLS-1$
  public static final String CRC = "crc"; //$NON-NLS-1$

  public void setInfo(Properties p) {
    name = p.getProperty(NAME,"???"); //$NON-NLS-1$
    id = p.getProperty(ID,id);
    setStatus(new SimpleStatus(
                    "true".equals(p.getProperty(LOOKING)), //$NON-NLS-1$
                    "true".equals(p.getProperty(AWAY)), //$NON-NLS-1$
                    p.getProperty(PROFILE,""), //$NON-NLS-1$
                    p.getProperty(CLIENT, ""), //$NON-NLS-1$
                    p.getProperty(IP, ""), //$NON-NLS-1$
                    p.getProperty(MODULE_VERSION, ""), //$NON-NLS-1$
                    p.getProperty(CRC, ""))); //$NON-NLS-1$
  }

  public Properties toProperties() {
    final Properties p1 = new Properties();
    if (name != null) {
      p1.put(NAME,name);
    }
    final SimpleStatus status = (SimpleStatus)getStatus();
    p1.put(LOOKING, String.valueOf(status.isLooking()));
    p1.put(AWAY, String.valueOf(status.isAway()));
    final String profile = status.getProfile();
    if (profile != null) {
      p1.put(PROFILE,profile);
    }
    final String client = status.getClient();
    if (client != null) {
      p1.put(CLIENT,client);
    }
    final String ip = status.getIp();
    if (ip != null) {
      p1.put(IP,ip);
    }
    final String moduleVersion = status.getModuleVersion();
    if (moduleVersion != null) {
      p1.put(MODULE_VERSION,moduleVersion);
    }
    final String crc = status.getCrc();
    if (ip != null) {
      p1.put(CRC,crc);
    }
    Properties p = p1;
    p.put(ID,id);
    return p;
  }
}
