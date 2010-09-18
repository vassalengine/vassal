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
package VASSAL.chat.jabber;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
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
public class JabberClientFactory extends ChatServerFactory {
  private static final Logger logger =
    LoggerFactory.getLogger(JabberClientFactory.class);

  public static final String JABBER_SERVER_TYPE = "jabber"; //$NON-NLS-1$
  public static final String JABBER_PWD = "jabberPassword"; //$NON-NLS-1$
  public static final String JABBER_LOGIN = "jabberLogin"; //$NON-NLS-1$
  public static final String JABBER_PORT = "jabberPort"; //$NON-NLS-1$
  public static final String JABBER_HOST = "jabberHost"; //$NON-NLS-1$
  public static final String DEFAULT_JABBER_PORT = "5222"; //$NON-NLS-1$
  public static final String DEFAULT_JABBER_HOST = "localhost"; //$NON-NLS-1$

  public ChatServerConnection buildServer(Properties serverConfig) {
    String host = serverConfig.getProperty(JABBER_HOST, DEFAULT_JABBER_HOST);
    int port = 5222;
    try {
      port = Integer.parseInt(serverConfig.getProperty(JABBER_PORT, DEFAULT_JABBER_PORT));
    }
    // FIXME: review error message
    catch (NumberFormatException e) {
      e.printStackTrace();
    }
    ModuleAccountInfo account = new ModuleAccountInfo(serverConfig.getProperty(JABBER_LOGIN),serverConfig.getProperty(JABBER_PWD));
    JabberClient client = new JabberClient(GameModule.getGameModule(), host, port, account);
    client.addPropertyChangeListener(ChatServerConnection.STATUS, new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        final String mess = (String) evt.getNewValue();
        GameModule.getGameModule().warn(mess);
        logger.error("", mess);
      }
    });
    client.addPropertyChangeListener(ChatServerConnection.INCOMING_MSG, new CommandDecoder());
    return client;
  }
}
