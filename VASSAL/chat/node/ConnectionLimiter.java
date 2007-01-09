/*
 * $Id: ConnectionLimiter.java,v 1.1 2004-11-28 21:50:55 rkinney Exp $
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

import java.util.Map;
import java.util.HashMap;

/**
 * Limits connections to the server to one per registered username
 */
public class ConnectionLimiter {
  private Map connections = new HashMap();
  public synchronized void register(String name, SocketHandler handler) {
    if (connections.containsKey(name)) {
      kickOff((SocketHandler) connections.get(name));
    }
    connections.put(name,handler);
  }

  private void kickOff(SocketHandler handler) {
    handler.writeLine("CHATOnly one connection per username allowed.  Goodbye.");
    handler.close();
  }
}
