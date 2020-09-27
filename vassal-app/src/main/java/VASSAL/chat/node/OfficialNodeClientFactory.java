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
import VASSAL.chat.HttpMessageServer;
import VASSAL.chat.peer2peer.PeerPoolInfo;
import VASSAL.i18n.Resources;

public class OfficialNodeClientFactory extends NodeClientFactory {
  public static final String OFFICIAL_TYPE = "official"; //$NON-NLS-1$
  public static final String OFFICIAL_HOST = "game.vassalengine.org"; //NON-NLS
  public static final String OFFICIAL_PORT = "5050";

  private static final String UNNAMED_MODULE = Resources.getString("Chat.unknown_module");  //$NON-NLS-1$
  private static final String UNKNOWN_USER = Resources.getString("Chat.unknown_user");  //$NON-NLS-1$

  @Override
  protected ChatServerConnection buildServerImpl(Properties param) {
    final String host = param.getProperty(NODE_HOST, OFFICIAL_HOST);  //$NON-NLS-1$
    final int port = Integer.parseInt(param.getProperty(NODE_PORT, OFFICIAL_PORT));  //$NON-NLS-1$

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

    return new OfficialNodeClient(
      g.getGameName(),
      GameModule.getUserId() + "." + System.currentTimeMillis(),
      g,
      host,
      port,
      httpMessageServer,
      httpMessageServer
    );
  }
}
