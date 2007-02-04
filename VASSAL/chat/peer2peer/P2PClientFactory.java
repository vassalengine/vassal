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
package VASSAL.chat.peer2peer;

import java.util.Properties;
import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.ChatServerFactory;
import VASSAL.chat.HttpMessageServer;

/**
 * @author rkinney
 */
public class P2PClientFactory extends ChatServerFactory {
  public static final String P2P_TYPE="peer2peer";

  public ChatServerConnection buildServer(Properties param) {
    HttpMessageServer httpMessageServer = new HttpMessageServer(new PeerPoolInfo() {
        public String getModuleName() {
          return GameModule.getGameModule() == null ? "<unknown module>" : GameModule.getGameModule().getGameName();
        }

        public String getUserName() {
          return GameModule.getUserId();
        }
      
    });
    return new P2PClient(GameModule.getGameModule(),httpMessageServer,httpMessageServer,new DirectPeerPool());
  }
}
