/*
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

import java.awt.AWTError;
import java.awt.Font;
import java.awt.Toolkit;
import java.lang.reflect.InvocationTargetException;
import javax.swing.UIManager;
import javax.swing.SwingUtilities;
import javax.swing.UnsupportedLookAndFeelException;

import VASSAL.preferences.Prefs;
import VASSAL.preferences.ReadOnlyPrefs;
import org.apache.commons.lang3.SystemUtils;

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

  /**
   * Changes all the UI fonts to the specified one
   * @param f Font for UI
   */
  public void setUIFont(javax.swing.plaf.FontUIResource f) {
    final java.util.Enumeration keys = UIManager.getDefaults().keys();
    while (keys.hasMoreElements()) {
      final Object key = keys.nextElement();
      final Object value = UIManager.get(key);
      if (value instanceof javax.swing.plaf.FontUIResource) {
        UIManager.put(key, f);
      }
    }
  }


  protected void initUIProperties() {
    System.setProperty("swing.aatext", "true"); //$NON-NLS-1$ //$NON-NLS-2$
    System.setProperty("swing.boldMetal", "false"); //$NON-NLS-1$ //$NON-NLS-2$
    System.setProperty("awt.useSystemAAFontSettings", "on"); //$NON-NLS-1$ //$NON-NLS-2$

    // A bad .accessibility.properties file (on Windows) can cause Swing to
    // throw an AWTError. Some screen readers will install this without
    // installing a proper Java Accessibility Bridge to make use of it. This
    // is a rare, but perplexing error for the user so we give some specific
    // advice about how to deal with it in the error message.
    try {
      Toolkit.getDefaultToolkit();
    }
    catch (AWTError e) {
      ErrorDialog.show(
        e,
        "Error.assistive_technology_not_found"
      );
    }

    try {
      SwingUtilities.invokeAndWait(() -> {
        if (SystemUtils.IS_OS_MAC) {
          // Bug 2505: JEditorPane.registerEditorKitForContentType()
          // sometimes throws an NPE because the context class loader is
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
          catch (ClassNotFoundException | UnsupportedLookAndFeelException
            | InstantiationException | IllegalAccessException e) {
            ErrorDialog.bug(e);
          }

          // The GTK LaF has a color picker which lacks the ability to
          // select transparency. We can override that and it doesn't
          // look too goofy.
          if ("com.sun.java.swing.plaf.gtk.GTKLookAndFeel".equals(//NON-NLS
            UIManager.getLookAndFeel().getClass().getName())) {
            UIManager.put(
              "ColorChooserUI",
              "javax.swing.plaf.basic.BasicColorChooserUI"
            );
          }
        }

        // Ensure consistent behavior in NOT consuming "mousePressed" events
        // upon a JPopupMenu closing (added for Windows L&F, but others might
        // also be affected.
        UIManager.put("PopupMenu.consumeEventOnClose", Boolean.FALSE);

        // If user has chosen a different UI font size, then put that into effect
        final String fontString = ReadOnlyPrefs.getGlobalPrefs().getStoredValue(Prefs.OVERRIDE_DEFAULT_FONT_SIZE);
        try {
          final int fontSize = Integer.parseInt(fontString);
          if (fontSize > 0) {
            setUIFont(new javax.swing.plaf.FontUIResource("SansSerif", Font.PLAIN, Math.max(8, Math.min(fontSize, 32))));
          }
        }
        catch (NumberFormatException e) {
          // No action, keep default system/java/whatever fonts.
        }
      });
    }
    catch (InterruptedException | InvocationTargetException e) {
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
