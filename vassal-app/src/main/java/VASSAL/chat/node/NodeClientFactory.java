/*
 *
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.ChatServerFactory;
import VASSAL.chat.CommandDecoder;
import VASSAL.chat.HttpMessageServer;
import VASSAL.chat.peer2peer.PeerPoolInfo;
import VASSAL.i18n.Resources;

/**
 * @author rkinney
 */
public class NodeClientFactory extends ChatServerFactory {
  private static final Logger logger =
    LoggerFactory.getLogger(NodeClientFactory.class);

  private static final String UNNAMED_MODULE = Resources.getString("Chat.unknown_module");  //$NON-NLS-1$
  private static final String UNKNOWN_USER = Resources.getString("Chat.unknown_user");  //$NON-NLS-1$
  public static final String NODE_TYPE = "node";  //$NON-NLS-1$
  public static final String NODE_HOST = "nodeHost";  //$NON-NLS-1$
  public static final String NODE_PORT = "nodePort";  //$NON-NLS-1$

  public NodeClientFactory() {
  }

  @Override
  public ChatServerConnection buildServer(Properties param) {
    final String host = param.getProperty(NODE_HOST, "game.vassalengine.org");  //$NON-NLS-1$
    final int port = Integer.parseInt(param.getProperty(NODE_PORT, "5050"));  //$NON-NLS-1$

    final NodeServerInfo nodeServerInfo = new NodeServerInfo() {
      @Override
      public String getHostName() {
        return host;
      }

      @Override
      public int getPort() {
        return port;
      }
    };

    final PeerPoolInfo publicInfo = new PeerPoolInfo() {
      @Override
      public String getModuleName() {
        final GameModule g = GameModule.getGameModule();
        return g == null ? UNNAMED_MODULE : g.getGameName();
      }

      @Override
      public String getUserName() {
        final GameModule g = GameModule.getGameModule();
        return g == null ? UNKNOWN_USER : (String) g.getPrefs().getValue(GameModule.REAL_NAME);
      }
    };

    final HttpMessageServer httpMessageServer = new HttpMessageServer(publicInfo);
    final GameModule g = GameModule.getGameModule();

    final SocketNodeClient server = new SocketNodeClient(
      g.getGameName(),
      GameModule.getUserId() + "." + System.currentTimeMillis(),
      g,
      nodeServerInfo,
      httpMessageServer,
      httpMessageServer
    );

    g.getPrefs().getOption(GameModule.REAL_NAME).fireUpdate();
    g.getPrefs().getOption(GameModule.PERSONAL_INFO).fireUpdate();

    server.addPropertyChangeListener(ChatServerConnection.STATUS, e -> {
      final String mess = (String) e.getNewValue();
      GameModule.getGameModule().warn(mess);
      logger.error("", mess);
    });

    server.addPropertyChangeListener(ChatServerConnection.INCOMING_MSG, new CommandDecoder());

    return server;
  }
}
