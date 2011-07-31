/*
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

import VASSAL.build.module.Chatter;


/**
 * Simple Player bean
 */
public class SimplePlayer implements VASSAL.chat.Player {
  protected String id;
  protected String name;
  protected PlayerStatus status;

  public SimplePlayer(String id, String name, PlayerStatus status) {
    super();
    this.id = id;
    this.name = name;
    this.status = status;
  }

  public SimplePlayer(String name) {
    this(name,name,new SimpleStatus());
  }

  public SimplePlayer() {
    this((String) null);
  }

  public String toString() {
    return name;
  }

  public String getName() {
    if (name == null || name.length() == 0 || name.trim().length() == 0 || name.equals("<nobody>")) {  //$NON-NLS-1$
      return "("+Chatter.getAnonymousUserName()+")";  //$NON-NLS-1$  //$NON-NLS-2$
    }
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public boolean equals(Object o) {
    if (o instanceof Player) {
      return id != null && id.equals(((Player) o).getId());
    }
    else {
      return false;
    }
  }

  public String getId() {
    return id;
  }

  public PlayerStatus getStatus() {
    return status;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setStatus(PlayerStatus status) {
    this.status = status;
  }

  public void updateStatus() {
    if (status instanceof SimpleStatus) {
      ((SimpleStatus) status).updateStatus();
    }
  }
}
