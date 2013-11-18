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
import VASSAL.chat.messageboard.Message;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.i18n.Resources;

/**
 * @author rkinney
 */
public class P2PClientFactory extends ChatServerFactory {
  private static final Logger logger =
    LoggerFactory.getLogger(ChatServerFactory.class);

  public static final String P2P_TYPE = "peer2peer"; //$NON-NLS-1$
  public static final String P2P_LISTEN_PORT = "listenPort"; //$NON-NLS-1$
  public static final String P2P_MODE_KEY = "mode"; //$NON-NLS-1$
  public static final String P2P_SERVER_MODE = "server"; //$NON-NLS-1$
  public static final String P2P_CLIENT_MODE = "client"; //$NON-NLS-1$
  public static final String P2P_SERVER_IP = "serverIp"; //$NON-NLS-1$
  public static final String P2P_SERVER_PORT = "serverPort"; //$NON-NLS-1$
  public static final String P2P_SERVER_NAME = "serverName"; //$NON-NLS-1$
  public static final String P2P_SERVER_PW = "serverPw"; //$NON-NLS-1$

  public ChatServerConnection buildServer(Properties param) {

    final HttpMessageServer httpMessageServer = new P2PMessageServer();

    final P2PClient server = new P2PClient(GameModule.getGameModule(),httpMessageServer,httpMessageServer,new DirectPeerPool(param), param);
    server.addPropertyChangeListener(ChatServerConnection.STATUS, new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        final String mess = (String) evt.getNewValue();
        GameModule.getGameModule().warn(mess);
        logger.info(mess);
      }
    });
    server.addPropertyChangeListener(ChatServerConnection.INCOMING_MSG, new CommandDecoder());
    return server;
  }

  class P2PMessageServer extends HttpMessageServer {

    public P2PMessageServer() {
      super(new PeerPoolInfo() {
        public String getModuleName() {
          return GameModule.getGameModule() == null ? Resources.getString("Chat.unknown_module") : GameModule.getGameModule().getGameName(); //$NON-NLS-1$
        }

        public String getUserName() {
          return GameModule.getUserId();
        }
      });
    }

    public Command getWelcomeMessage() {
      return new NullCommand();
    }

    public Message[] getMessages() {
      return null;
    }

    public void postMessage(String content) {
      return;
    }
  }
}
