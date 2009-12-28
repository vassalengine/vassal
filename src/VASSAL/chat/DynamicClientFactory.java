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

import java.util.Properties;

/**
 * @author rkinney
 */
public class DynamicClientFactory extends ChatServerFactory {
  public static final String DYNAMIC_TYPE = "dynamic"; //$NON-NLS-1$
  public static final String URL = "url"; //$NON-NLS-1$

  public ChatServerConnection buildServer(Properties param) {
    String url = param.getProperty(URL);
    DynamicClient client = url == null ? new DynamicClient() : new DynamicClient(url);
    if (param.containsKey(DYNAMIC_TYPE)) {
      //param = new Properties(param);
      // Force the type of the dynamically created server proxy
      param.setProperty(ChatServerFactory.TYPE_KEY, param.getProperty(DYNAMIC_TYPE));
    }
    client.setOverrides(param);
    return client;
  }
}
