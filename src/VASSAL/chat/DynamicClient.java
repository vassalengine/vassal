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
package VASSAL.chat;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;
import java.util.Properties;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;

/**
 * Determines server implementation at run-time by downloading properties from the vassalengine.org site. Refreshes
 * every time the user attempts to connect
 * 
 * @author rkinney
 * 
 */
public class DynamicClient extends HybridClient {
  private String serverConfigURL;

  public DynamicClient() {
    this("http://www.vassalengine.org/util/getServerImpl"); //$NON-NLS-1$
  }

  public DynamicClient(String serverConfigURL) {
    this.serverConfigURL = serverConfigURL;
  }

  protected ChatServerConnection buildDelegate() {
    ChatServerConnection c = null;
    try {
      Properties p = getServerConfig();
      c = ChatServerFactory.build(p);
    }
    catch (IOException e) {
      e.printStackTrace();
      fireStatus("Unable to initiate connection to server"); //$NON-NLS-1$
    }
    return c;
  }

  private Properties getServerConfig() throws IOException {
    HttpRequestWrapper r = new HttpRequestWrapper(serverConfigURL);
    Properties p = new Properties();
    p.put("module", GameModule.getGameModule() == null ? "Test" : GameModule.getGameModule().getGameName()); //$NON-NLS-1$ //$NON-NLS-2$
    p.put("vassalVersion", VASSAL.Info.getVersion()); //$NON-NLS-1$
    List<String> l = r.doGet(p);
    if (l.isEmpty()) {
      throw new IOException(Resources.getString("Server.empty_response")); //$NON-NLS-1$
    }
    p = new Properties();

    final StringBuilder buff = new StringBuilder();
    for (String s : l) {
      buff.append(s).append('\n');
    }
    p.load(new ByteArrayInputStream(buff.toString().getBytes()));
    return p;
  }

  public void setConnected(boolean connect) {
    if (connect && !isConnected()) {
      setDelegate(buildDelegate());
    }
    super.setConnected(connect);
    if (!connect && !isConnected()) {
      setDelegate(new DummyClient());
    }
  }
}
