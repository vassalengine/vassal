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

import java.lang.reflect.InvocationTargetException;
import javax.swing.UIManager;
import javax.swing.SwingUtilities;
import javax.swing.UnsupportedLookAndFeelException;

import org.apache.commons.lang.SystemUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.tools.ErrorDialog;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class StartUp {
  private static final Logger logger = LoggerFactory.getLogger(StartUp.class);

  public void initSystemProperties() {
    initHTTPProxyProperties();
    initSystemSpecificProperties();
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

    try {
      SwingUtilities.invokeAndWait(new Runnable() {
        public void run() {
          if (SystemUtils.IS_OS_MAC_OSX) {
            // Bug 2505: JEditorPane.registerEditorKitForContentType()
            // sometimes throws an NPE becuase the context class loader is
            // null. This is a JDK bug, but we can work around it by setting
            // the EDT's context ClassLoader explicitly.
            Thread.currentThread().setContextClassLoader(
              ClassLoader.getSystemClassLoader()
            );
          }

          if (!SystemUtils.IS_OS_WINDOWS) {
            // use native LookAndFeel
            // NB: This must be after Mac-specific properties
            try {
              UIManager.setLookAndFeel(
                UIManager.getSystemLookAndFeelClassName()
              );
            }
            catch (ClassNotFoundException e) {
              ErrorDialog.bug(e);
            }
            catch (IllegalAccessException e) {
              ErrorDialog.bug(e);
            }
            catch (InstantiationException e) {
              ErrorDialog.bug(e);
            }
            catch (UnsupportedLookAndFeelException e) {
              ErrorDialog.bug(e);
            }
          }

          // Ensure consistent behavior in NOT consuming "mousePressed" events
          // upon a JPopupMenu closing (added for Windows L&F, but others might
          // also be affected.
          UIManager.put("PopupMenu.consumeEventOnClose", Boolean.FALSE);
        }
      });
    }
    catch (InterruptedException e) {
      ErrorDialog.bug(e);
    }
    catch (InvocationTargetException e) {
      ErrorDialog.bug(e);
    }
  }

  protected void initSystemSpecificProperties() {}

  public void startErrorLog() {
    // begin the error log
    logger.info("Starting"); //$NON-NLS-1$
    logger.info("OS " + System.getProperty("os.name") + " " + System.getProperty("os.version")); //$NON-NLS-1$ //$NON-NLS-2$
    logger.info("Java version " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$
    logger.info("VASSAL version " + Info.getVersion()); //$NON-NLS-1$
  }
}
