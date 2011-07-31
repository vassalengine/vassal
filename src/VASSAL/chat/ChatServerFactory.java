/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

/**
 * A Factory for building {@link ChatServerConnection} instances
 *
 * @author rkinney
 */
public abstract class ChatServerFactory {
  public static final String TYPE_KEY = "type"; //$NON-NLS-1$
  private static Map<String, ChatServerFactory> factories =
    new HashMap<String,ChatServerFactory>();

  public abstract ChatServerConnection buildServer(Properties param);

  public static void register(String key, ChatServerFactory instance) {
    factories.put(key, instance);
  }

  public static ChatServerConnection build(Properties param) {
    String type = param.getProperty(TYPE_KEY);
    ChatServerFactory factory = factories.get(type);
    return factory.buildServer(param);
  }
}
