/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.chat.node;

import java.util.HashMap;
import java.util.Map;

import VASSAL.i18n.Resources;

/**
 * Limits connections to the server to one per registered username
 */
public class ConnectionLimiter {
  private Map<String,SocketHandler> connections =
    new HashMap<String,SocketHandler>();

  public synchronized void register(String name, SocketHandler handler) {
    if (connections.containsKey(name)) {
      kickOff(connections.get(name));
    }
    connections.put(name, handler);
  }

  private void kickOff(SocketHandler handler) {
    handler.writeLine(Resources.getString("Chat.too_many")); //$NON-NLS-1$
    handler.close();
  }
}
