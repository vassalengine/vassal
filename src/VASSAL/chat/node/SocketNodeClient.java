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
package VASSAL.chat.node;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Properties;

import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.command.CommandEncoder;
import VASSAL.i18n.Resources;

public class SocketNodeClient extends NodeClient implements SocketWatcher {
  private SocketHandler sender;
  protected NodeServerInfo serverInfo;

  public SocketNodeClient(String moduleName, String playerId, CommandEncoder encoder, NodeServerInfo serverInfo, MessageBoard msgSvr, WelcomeMessageServer welcomer) {
    super(moduleName, playerId, encoder, msgSvr, welcomer);
    this.serverInfo = serverInfo;
    serverStatus = new CgiServerStatus();
  }

  public SocketNodeClient(String moduleName, String playerId, CommandEncoder encoder, final String host, final int port, MessageBoard msgSvr, WelcomeMessageServer welcomer) {
    this(moduleName, playerId, encoder, new NodeServerInfo() {

      public String getHostName() {
        return host;
      }

      public int getPort() {
        return port;
      }

    }, msgSvr, welcomer);

  }

  public void send(String command) {
    sender.writeLine(command);
  }

  protected void initializeConnection() throws UnknownHostException, IOException {
    Socket s = new Socket(serverInfo.getHostName(), serverInfo.getPort());
    sender = new BufferedSocketHandler(s, this);
    sender.start();

  }

  protected void closeConnection() {
    SocketHandler s = sender;
    sender = null;
    s.close();
  }

  public boolean isConnected() {
    return sender != null;
  }

  public void socketClosed(SocketHandler handler) {
    if (sender != null) {
      propSupport.firePropertyChange(STATUS, null, Resources.getString("Server.lost_connection")); //$NON-NLS-1$
      propSupport.firePropertyChange(CONNECTED, null, Boolean.FALSE);
      sender = null;
    }
  }

  public void handleMessage(String msg) {
    handleMessageFromServer(msg);
  }

  public void updateConfig(Properties params) {
    // No Configurable parameters
  }
}
