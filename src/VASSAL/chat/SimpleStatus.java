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
package VASSAL.chat;

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

  public SimpleStatus(boolean looking, boolean away, String profile) {
    this(looking, away, profile, "", "", "", "");
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
}
