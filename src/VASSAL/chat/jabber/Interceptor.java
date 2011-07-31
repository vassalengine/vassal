/*
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

import org.jivesoftware.smack.PacketInterceptor;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.filter.PacketFilter;
import org.jivesoftware.smack.packet.Packet;

/**
 * Convenience class that combines a packet filter and interceptor, with logging of untrapped runtime exceptions
 * @author rodneykinney
 *
 */
public abstract class Interceptor implements PacketInterceptor, PacketFilter {
  public void interceptPacket(Packet packet) {
    try {
      intercept(packet);
    }
    // FIXME: review error message
    catch (RuntimeException e) {
      e.printStackTrace();
    }
  }

  protected abstract void intercept(Packet p);

  public boolean accept(Packet packet) {
    try {
      return acceptPacket(packet);
    }
    // FIXME: review error message
    catch (RuntimeException e) {
      e.printStackTrace();
      return false;
    }
  }

  protected abstract boolean acceptPacket(Packet packet);

  public void addTo(XMPPConnection conn) {
    conn.addPacketWriterInterceptor(this, this);
  }
}
