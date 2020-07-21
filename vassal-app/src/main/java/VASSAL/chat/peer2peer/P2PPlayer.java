/*
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
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

import java.io.IOException;
import java.util.Properties;

import org.litesoft.p2pchat.PeerInfo;

import VASSAL.chat.Player;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleStatus;
import VASSAL.tools.PropertiesEncoder;

public class P2PPlayer extends SimplePlayer {
  private static final String ID = "id"; //$NON-NLS-1$
  private static final String ROOM = "room"; //$NON-NLS-1$

  private PeerInfo info;
  private Properties props;

  public P2PPlayer(PeerInfo info) {
    this.info = info;
    if (info.getChatName() != null) {
      try {
        props = new PropertiesEncoder(info.getChatName()).getProperties();
        setStats();
      }
      // FIXME: review error message
      catch (IOException ex) {
        props = new Properties();
        setProps();
      }
    }
    else {
      props = new Properties();
      setProps();
    }
  }

  public void setStats(Player p) {
    setName(p.getName());
    setStatus(p.getStatus());
    setId(p.getId());
    setProps();
  }

  private void setProps() {
    final SimpleStatus s = (SimpleStatus) status;
    props.put(SimpleStatus.NAME, getName());
    props.put(SimpleStatus.LOOKING, String.valueOf(s.isLooking()));
    props.put(SimpleStatus.AWAY, String.valueOf(s.isAway()));
    props.put(SimpleStatus.PROFILE, s.getProfile());
    props.put(SimpleStatus.IP, s.getIp());
    props.put(SimpleStatus.CLIENT, s.getClient());
    props.put(SimpleStatus.MODULE_VERSION, s.getModuleVersion());
    props.put(SimpleStatus.CRC, s.getCrc());
    info.setChatName(new PropertiesEncoder(props).getStringValue());
  }

  public void setProperty(String key, String value) {
    props.setProperty(key, value);
  }

  private void setStats() {
    setName(props.getProperty(SimpleStatus.NAME, "???")); //$NON-NLS-1$
    setStatus(
        new SimpleStatus(
            "true".equals(props.getProperty(SimpleStatus.LOOKING)), //$NON-NLS-1$
            "true".equals(props.getProperty(SimpleStatus.AWAY)), //$NON-NLS-1$
            props.getProperty(SimpleStatus.PROFILE, ""), //$NON-NLS-1$
            props.getProperty(SimpleStatus.CLIENT, ""), //$NON-NLS-1$
            props.getProperty(SimpleStatus.IP, ""), //$NON-NLS-1$
            props.getProperty(SimpleStatus.MODULE_VERSION, ""), //$NON-NLS-1$
            props.getProperty(SimpleStatus.CRC, "")));  //$NON-NLS-1$
  }

  public String getRoom() {
    return props.getProperty(ROOM);
  }

  public void setRoom(String name) {
    props.put(ROOM, name);
    setProps();
  }

  @Override
  public String getId() {
    return props.getProperty(ID);
  }

  @Override
  public void setId(String id) {
    props.put(ID,id);
    setProps();
  }

  public boolean equals(Object o) {
    if (o instanceof P2PPlayer) {
      P2PPlayer p = (P2PPlayer) o;
      return getId() == null ? info.equals(p.info) : getId().equals(p.getId());
    }
    else {
      return false;
    }
  }

  public PeerInfo getInfo() {
    return info;
  }

  public String summary() {
    return getName() + " [looking = " + ((SimpleStatus)status).isLooking() + ", away = " + ((SimpleStatus)getStatus()).isAway() + ", room = " + props.getProperty(ROOM) + ", host = " + getInfo().getAddresses() + "]"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
  }
}
