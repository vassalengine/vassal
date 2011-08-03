/*
 * $Id$
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
import java.util.List;
import java.util.Properties;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.DynamicClient;
import VASSAL.chat.HttpRequestWrapper;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.messageboard.Message;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.peer2peer.DirectPeerPool;
import VASSAL.chat.peer2peer.P2PClient;
import VASSAL.chat.peer2peer.PeerPoolInfo;
import VASSAL.command.CommandEncoder;
import VASSAL.i18n.Resources;

/**
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: May 29, 2003
 */
public class NodeHybridClient extends DynamicClient {
  private String addressURL;
  private MessageBoard msgSvr;
  private WelcomeMessageServer welcomeMsgSvr;
  private PeerPoolInfo info;
  private CommandEncoder encoder;
  private ServerStatus status;

  public NodeHybridClient(CommandEncoder encoder, PeerPoolInfo info, MessageBoard msgSvr, WelcomeMessageServer welcomeMsgSvr, String addressURL) {
    this.addressURL = addressURL;
    this.encoder = encoder;
    this.info = info;
    this.msgSvr = msgSvr;
    this.welcomeMsgSvr = welcomeMsgSvr;
  }

  public void postMessage(String msg) {
    msgSvr.postMessage(msg);
  }

  public Message[] getMessages() {
    return msgSvr.getMessages();
  }

  public MessageBoard getMessageServer() {
    return msgSvr;
  }

  protected ChatServerConnection buildDelegate() {
    ChatServerConnection c = null;
    try {
      String address = getAddressFromURL();
      int index = address.indexOf(':');  //$NON-NLS-1$
      if (index < 0) {
        fireStatus(Resources.getString("Server.bad_address", address));  //$NON-NLS-1$
      }
      else {
        try {
          int port = Integer.parseInt(address.substring(index + 1));
          address = address.substring(0, index);
          c = new SocketNodeClient(info.getModuleName(), info.getUserName(),encoder, address, port, msgSvr, welcomeMsgSvr);
        }
        // FIXME: review error message
        catch (NumberFormatException ex) {
          fireStatus(Resources.getString("Server.bad_address2", address));   //$NON-NLS-1$
        }
      }
    }
    // FIXME: review error message
    catch (IOException e) {
      fireStatus(Resources.getString("Server.bad_address3"));  //$NON-NLS-1$
    }
    if (c == null) {
      fireStatus(Resources.getString("Server.peer_to_peer"));  //$NON-NLS-1$
      c = new P2PClient(encoder, msgSvr, welcomeMsgSvr, new DirectPeerPool());
    }
    return c;
  }

  private String getAddressFromURL() throws IOException {
    HttpRequestWrapper r = new HttpRequestWrapper(addressURL);
    Properties p = new Properties();
    p.put("module",info.getModuleName());  //$NON-NLS-1$
    p.put("vassalVersion",VASSAL.Info.getVersion());  //$NON-NLS-1$
    List<String> l = r.doGet(p);
    if (l.isEmpty()) {
      throw new IOException(Resources.getString("Server.empty_response"));  //$NON-NLS-1$
    }
    return l.get(0);
  }

  public ServerStatus getStatusServer() {
    return status;
  }

  public void setStatusServer(ServerStatus s) {
    status = s;
  }
}
