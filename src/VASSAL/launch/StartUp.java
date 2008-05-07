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

import java.io.IOException;
import java.io.File;
import java.io.FileOutputStream;
import java.io.PrintStream;
import javax.swing.UIManager;

import VASSAL.Info;
import VASSAL.tools.ErrorLog;

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

  public void setupErrorLog() {
    //
    // Error log setup
    //
    String stderr = "stderr";           //$NON-NLS-1$
    final String errorLog = "errorLog"; //$NON-NLS-1$

    // redirect stderr to errorLog by default
    if (System.getProperty(stderr) == null) {
      System.setProperty(stderr,
        new File(Info.getHomeDir(), errorLog).getPath());
    }

    // write no log if stderr is set to null
    stderr = System.getProperty(stderr); 
    if (!"null".equals(stderr)) { //$NON-NLS-1$
      try {
        System.setErr(new PrintStream(new FileOutputStream(stderr)));
      }
      catch (IOException ex) {
        System.err.println(
          "Unable to redirect stderr to " + stderr); //$NON-NLS-1$
      }
    }
  }

  public void startErrorLog() {
    // begin the error log
    System.err.println("-- OS " + System.getProperty("os.name")); //$NON-NLS-1$ //$NON-NLS-2$
    System.err.println("-- Java version " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$
    System.err.println("-- VASSAL version " + Info.getVersion()); //$NON-NLS-1$
    System.err.println("-- VASSAL instance " + Info.getInstanceID()); //$NON-NLS-1$
  }
}
