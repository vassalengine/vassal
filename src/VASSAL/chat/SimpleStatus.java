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

import java.net.InetAddress;
import java.net.UnknownHostException;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.tools.SequenceEncoder;

/**
 * Immutable PlayerStatus class with flags indicating "looking for a game" and "away from keyboard" and a String profile
 *
 * @author rkinney
 *
 */
public class SimpleStatus implements PlayerStatus {

  public static final String CRC = "crc"; //$NON-NLS-1$
  public static final String MODULE_VERSION = "moduleVersion"; //$NON-NLS-1$
  public static final String IP = "ip"; //$NON-NLS-1$
  public static final String CLIENT = "client"; //$NON-NLS-1$
  public static final String PROFILE = "profile"; //$NON-NLS-1$
  public static final String AWAY = "away"; //$NON-NLS-1$
  public static final String LOOKING = "looking"; //$NON-NLS-1$
  public static final String NAME = "name"; //$NON-NLS-1$

  private boolean looking;
  private boolean away;
  private String profile;
  private String client;
  private String ip;
  private String moduleVersion;
  private String crc;

  public SimpleStatus() {
    this(false, false, ""); //$NON-NLS-1$
  }

  public SimpleStatus(boolean looking, boolean away) {
    this(looking, away, ""); //$NON-NLS-1$
  }

  public SimpleStatus(boolean looking, boolean away, String profile) {
    this(looking, away, profile, "", "", "", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
  }

  public SimpleStatus(boolean looking, boolean away, String profile, String client, String ip, String module, String crc) {
    this.looking = looking;
    this.away = away;
    this.profile = profile;
    this.client = client;
    this.ip = ip;
    this.moduleVersion = module;
    this.crc = crc;
  }

  public boolean isAway() {
    return away;
  }

  public boolean isLooking() {
    return looking;
  }

  public String getProfile() {
    return profile;
  }

  public String getClient() {
    return client;
  }

  public String getIp() {
    return ip;
  }

  public String getModuleVersion() {
    return moduleVersion;
  }

  public String getCrc() {
    return crc;
  }

  public static String encode(SimpleStatus s) {
    final SequenceEncoder se = new SequenceEncoder(',');
    se.append(s.looking);
    se.append(s.away);
    se.append(s.profile);
    se.append(s.client);
    se.append(s.ip);
    se.append(s.moduleVersion);
    se.append(s.crc);
    return se.getValue();
  }

  public static SimpleStatus decode(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    return new SimpleStatus(sd.nextBoolean(false), sd.nextBoolean(false),sd.nextToken(""), sd.nextToken(""),  //$NON-NLS-1$ //$NON-NLS-2$
        sd.nextToken(""), sd.nextToken(""), sd.nextToken(""));         //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }

  /**
   * Update variable parts of status
   */
  public void updateStatus() {
    final GameModule g = GameModule.getGameModule();
    profile = (String) g.getPrefs().getValue(GameModule.PERSONAL_INFO);
    client = Info.getVersion();
    ip = ""; //$NON-NLS-1$
    try {
      ip = InetAddress.getLocalHost().getHostAddress();
    }
    catch (UnknownHostException e) {
    }
    moduleVersion = g.getGameVersion() + ((g.getArchiveWriter() == null) ? "" : " (Editing)");  //$NON-NLS-1$ //$NON-NLS-2$
    crc = Long.toHexString(g.getCrc());
  }
}
