/*
 * Copyright (c) 2000-2020 by Rodney Kinney, Brent Easton, Joel Uckelman
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

import java.util.Properties;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;

public class PrivateNodeClientFactory extends NodeClientFactory {
  public static final String PRIVATE_TYPE = "private"; //$NON-NLS-1$
  public static final String PRIVATE_HOST = "localhost";
  public static final String PRIVATE_PORT = "5050";

  @Override
  protected ChatServerConnection buildServerImpl(Properties param) {
    final String host = param.getProperty(NODE_HOST, PRIVATE_HOST);
    int port;
    try {
      port = Integer.parseInt(param.getProperty(NODE_PORT, PRIVATE_PORT));
    }
    catch (final NumberFormatException e) {
      port = Integer.parseInt(PRIVATE_PORT);
    }

    final GameModule g = GameModule.getGameModule();

    return new PrivateNodeClient(
      g.getGameName(),
      GameModule.getUserId() + "." + System.currentTimeMillis(),
      g,
      host,
      port
    );
  }
}
