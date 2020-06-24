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
package VASSAL.chat.jabber;

import org.jivesoftware.smack.packet.Presence;

import VASSAL.chat.SimpleStatus;

public class JabberStatus extends SimpleStatus {
  public JabberStatus(SimpleStatus copy) {
    super(copy.isLooking(), copy.isAway(), null);
  }

  public Presence.Mode getAvailability() {
    Presence.Mode mode = Presence.Mode.away;
    if (isAway()) {
      mode = Presence.Mode.xa;
    }
    else if (isLooking()) {
      mode = Presence.Mode.chat;
    }
    return mode;
  }
}
