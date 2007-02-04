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
package VASSAL.chat.jabber;

import VASSAL.chat.SimplePlayer;

public class JabberPlayer extends SimplePlayer {
  private String jid;

  public JabberPlayer() {
    super();
  }

  public JabberPlayer(String name, String jid) {
    super(name);
    this.jid = jid;
  }

  public String getJid() {
    return jid;
  }
  
  public boolean equals(Object o) {
    return o instanceof JabberPlayer && jid.equals(((JabberPlayer)o).jid);
  }
  
  public int hashCode() {
    return jid.hashCode();
  }
}
