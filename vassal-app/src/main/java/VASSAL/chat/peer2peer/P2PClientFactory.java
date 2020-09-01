/*
 * $Id:
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
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
package VASSAL.chat.peer2peer;

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.ChatServerFactory;
import VASSAL.chat.CommandDecoder;
import VASSAL.chat.DummyMessageServer;
import VASSAL.i18n.Resources;

/**
 * @author rkinney
 */
public class P2PClientFactory extends ChatServerFactory {
  private static final Logger logger =
    LoggerFactory.getLogger(ChatServerFactory.class);

  public static final String P2P_TYPE = "peer2peer"; //$NON-NLS-1$
  public static final String P2P_LISTEN_PORT = "listenPort"; //$NON-NLS-1$
  public static final String P2P_SERVER_IP = "serverIp"; //$NON-NLS-1$
  public static final String P2P_SERVER_PORT = "serverPort"; //$NON-NLS-1$
  public static final String P2P_SERVER_NAME = "serverName"; //$NON-NLS-1$
  public static final String P2P_SERVER_PW = "serverPw"; //$NON-NLS-1$

  @Deprecated(since = "2020-08-17", forRemoval = true)
  public static final String P2P_MODE_KEY = "mode"; //$NON-NLS-1$
  @Deprecated(since = "2020-08-17", forRemoval = true)
  public static final String P2P_SERVER_MODE = "server"; //$NON-NLS-1$
  @Deprecated(since = "2020-08-16", forRemoval = true)
  public static final String P2P_CLIENT_MODE = "client"; //$NON-NLS-1$

  @Override
  public ChatServerConnection buildServer(Properties param) {

    final DummyMessageServer msgServer = new DummyMessageServer();

    final P2PClient server = new P2PClient(
      GameModule.getGameModule(),
      msgServer, msgServer,
      new DirectPeerPool(param),
      param
    );

    server.addPropertyChangeListener(ChatServerConnection.STATUS, e -> {
      final String mess = (String) e.getNewValue();
      GameModule.getGameModule().warn(mess);
      logger.info(mess);
    });
    server.addPropertyChangeListener(ChatServerConnection.INCOMING_MSG, new CommandDecoder());
    return server;
  }
}
