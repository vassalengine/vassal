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
package VASSAL.chat.node;

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.ChatServerFactory;
import VASSAL.chat.CommandDecoder;

/**
 * @author rkinney
 */
public abstract class NodeClientFactory extends ChatServerFactory {
  private static final Logger logger =
    LoggerFactory.getLogger(NodeClientFactory.class);

  public static final String NODE_TYPE = "node";  //$NON-NLS-1$
  public static final String NODE_HOST = "nodeHost";  //$NON-NLS-1$
  public static final String NODE_PORT = "nodePort";  //$NON-NLS-1$

  protected abstract ChatServerConnection buildServerImpl(Properties param);

  @Override
  public ChatServerConnection buildServer(Properties param) {
    final ChatServerConnection server = buildServerImpl(param);

    final GameModule g = GameModule.getGameModule();

    g.getPrefs().getOption(GameModule.REAL_NAME).fireUpdate();
    g.getPrefs().getOption(GameModule.PERSONAL_INFO).fireUpdate();

    server.addPropertyChangeListener(ChatServerConnection.STATUS, e -> {
      final String mess = (String) e.getNewValue();
      GameModule.getGameModule().warn("!<b>" + mess);
      logger.error(mess);
    });

    server.addPropertyChangeListener(
      ChatServerConnection.INCOMING_MSG, new CommandDecoder()
    );

    return server;
  }
}
