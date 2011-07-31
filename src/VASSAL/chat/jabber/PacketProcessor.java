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

import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.filter.PacketFilter;
import org.jivesoftware.smack.packet.Packet;

/**
 * Combined packet filter/listener.  Wraps methods in a try/catch so that runtime exceptions are not swallowed silently
 * @author rodneykinney
 *
 */
public abstract class PacketProcessor implements PacketListener, PacketFilter {

  public void processPacket(Packet packet) {
    try {
      process(packet);
    }
    // FIXME: review error message
    catch (RuntimeException e) {
      e.printStackTrace();
    }
  }

  protected abstract void process(Packet packet);

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
    conn.addPacketListener(this, this);
  }
}
