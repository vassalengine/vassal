/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman 
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

package VASSAL.launch;

import VASSAL.Info;
import VASSAL.tools.logging.LogOutputStreamAdapter;
import VASSAL.tools.logging.Logger;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class StartUp {
  public void initSystemProperties() {
    initHTTPProxyProperties();
    initUIProperties();
  }

  protected void initHTTPProxyProperties() {
    final String httpProxyHost = "http.proxyHost";  //$NON-NLS-1$
    final String proxyHost = "proxyHost";           //$NON-NLS-1$

    if (System.getProperty(httpProxyHost) == null && 
        System.getProperty(proxyHost) != null) {
      System.setProperty(httpProxyHost, System.getProperty(proxyHost));
    }

    final String httpProxyPort = "http.proxyPort"; //$NON-NLS-1$
    final String proxyPort = "proxyPort";          //$NON-NLS-1$

    if (System.getProperty(httpProxyPort) == null &&
        System.getProperty(proxyPort) != null) {
      System.setProperty(httpProxyPort, System.getProperty(proxyPort));
    }
  }

  protected void initUIProperties() {
    System.setProperty("swing.aatext", "true"); //$NON-NLS-1$ //$NON-NLS-2$
    System.setProperty("swing.boldMetal", "false"); //$NON-NLS-1$ //$NON-NLS-2$
    System.setProperty("awt.useSystemAAFontSettings", "on"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void startErrorLog() {
    if (System.getProperty("stderr") != null) {
      Logger.addLogListener(new LogOutputStreamAdapter(System.err));
    }
    // begin the error log
    Logger.log("-- Starting"); //$NON-NLS-1$
    Logger.log("-- OS " + System.getProperty("os.name")); //$NON-NLS-1$ //$NON-NLS-2$
    Logger.log("-- Java version " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$
    Logger.log("-- VASSAL version " + Info.getVersion()); //$NON-NLS-1$
  }
}
