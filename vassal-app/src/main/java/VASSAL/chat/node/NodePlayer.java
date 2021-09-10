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
import java.util.Objects;

import VASSAL.build.module.Chatter;
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

  @Override
  public String getId() {
    return id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof NodePlayer)) return false;

    final NodePlayer hPlayer = (NodePlayer) o;
    return Objects.equals(id, hPlayer.id);
  }

  @Override
  public int hashCode() {
    return (id != null ? id.hashCode() : 0);
  }

  public void setInfo(Properties p) {
    name = p.getProperty(SimpleStatus.NAME, "???"); //$NON-NLS-1$
    if (name == null || name.isBlank() || name.equals("[nobody]")) { //NON-NLS
      name = "(" + Chatter.getAnonymousUserName() + ")";
    }
    id = p.getProperty(ID, id);
    setStatus(new SimpleStatus(
                    "true".equals(p.getProperty(SimpleStatus.LOOKING)), //$NON-NLS-1$
                    "true".equals(p.getProperty(SimpleStatus.AWAY)), //$NON-NLS-1$
                    p.getProperty(SimpleStatus.PROFILE, ""), //$NON-NLS-1$
                    p.getProperty(SimpleStatus.CLIENT, ""), //$NON-NLS-1$
                    p.getProperty(SimpleStatus.IP, ""), //$NON-NLS-1$
                    p.getProperty(SimpleStatus.MODULE_VERSION, ""), //$NON-NLS-1$
                    p.getProperty(SimpleStatus.CRC, ""))); //$NON-NLS-1$
  }

  public Properties toProperties() {
    final Properties p1 = new Properties();
    if (name != null) {
      p1.put(SimpleStatus.NAME, name);
    }
    final SimpleStatus status = (SimpleStatus)getStatus();
    p1.put(SimpleStatus.LOOKING, String.valueOf(status.isLooking()));
    p1.put(SimpleStatus.AWAY, String.valueOf(status.isAway()));
    final String profile = status.getProfile();
    if (profile != null) {
      p1.put(SimpleStatus.PROFILE, profile);
    }
    final String client = status.getClient();
    if (client != null) {
      p1.put(SimpleStatus.CLIENT, client);
    }
    final String ip = status.getIp();
    if (ip != null) {
      p1.put(SimpleStatus.IP, ip);
    }
    final String moduleVersion = status.getModuleVersion();
    if (moduleVersion != null) {
      p1.put(SimpleStatus.MODULE_VERSION, moduleVersion);
    }
    final String crc = status.getCrc();
    if (ip != null) {
      p1.put(SimpleStatus.CRC, crc);
    }
    p1.put(ID, id == null ? "" : id);
    return p1;
  }
}
