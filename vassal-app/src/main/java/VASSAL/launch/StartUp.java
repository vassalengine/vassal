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
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.swing.UIManager;
import javax.swing.SwingUtilities;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.JOptionPane;
import javax.swing.LookAndFeel;

import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.ReadOnlyPrefs;
import org.apache.commons.lang3.StringUtils;
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
  public void setUIFont(javax.swing.plaf.FontUIResource f, javax.swing.plaf.FontUIResource fItalic, javax.swing.plaf.FontUIResource fBold, javax.swing.plaf.FontUIResource fBoth) {
    final java.util.Enumeration keys = UIManager.getDefaults().keys();
    while (keys.hasMoreElements()) {
      final Object key = keys.nextElement();
      final Object value = UIManager.get(key);
      if (value instanceof javax.swing.plaf.FontUIResource) {
        final javax.swing.plaf.FontUIResource fonty = (javax.swing.plaf.FontUIResource) value;
        UIManager.put(key, fonty.getStyle() == Font.ITALIC ? fItalic : fonty.getStyle() == Font.BOLD ? fBold : fonty.getStyle() == (Font.BOLD | Font.ITALIC) ? fBoth : f);
      }
    }
  }

  /**
   * Helper object to store Look and Feel loader errors.
   * Resource string lookup is not possible until the UIManager loads.
   * The object stores a resource ID and the associated exception
   * for display after the UI is functional.
   */
  protected static class LafLoadError {
    protected String id;
    protected Exception exception;

    public LafLoadError(String id, Exception e) {
      this.id = id;
      exception = e;
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
      Toolkit.getDefaultToolkit(); // NOPMD
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

        LafLoadError lafClassLoadError = null;
        String defaultLaf = System.getProperty("swing.defaultlaf"); // NON-NLS
        if (defaultLaf != null) {
          try {
            // Check that the look and feel is in the class path.
            Class.forName(defaultLaf, false, getClass().getClassLoader());
          }
          catch (ClassNotFoundException e) {
            // Attempt to dynamically load the look and feel class.
            final String lookAndFeelPath = System.getProperty("look.feel"); // NON-NLS
            if (!StringUtils.isBlank(lookAndFeelPath)) {
              lafClassLoadError = installLookAndFeel(defaultLaf, lookAndFeelPath);
            }
            else {
              // The desired Look and Feel is not available.
              // Record the error and report it later once the UIManager is up.

              lafClassLoadError = new LafLoadError("Startup.laf_no_path", e);  // NON-NLS
            }
            if (lafClassLoadError != null) {
              defaultLaf = null;
              System.clearProperty("swing.defaultlaf"); // NON-NLS
            }
          }
        }
        if (!SystemUtils.IS_OS_WINDOWS) {
          if (defaultLaf == null) {
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
            setUIFont(new javax.swing.plaf.FontUIResource("SansSerif", Font.PLAIN, Math.max(8, Math.min(fontSize, 32))),
                      new javax.swing.plaf.FontUIResource("SansSerif", Font.ITALIC, Math.max(8, Math.min(fontSize, 32))),
                      new javax.swing.plaf.FontUIResource("SansSerif", Font.BOLD, Math.max(8, Math.min(fontSize, 32))),
                      new javax.swing.plaf.FontUIResource("SansSerif", Font.BOLD | Font.ITALIC, Math.max(8, Math.min(fontSize, 32))));
          }
        }
        catch (NumberFormatException e) {
          // No action, keep default system/java/whatever fonts.
        }
        // Report any Look and Feel load errors.
        if (lafClassLoadError != null) {
          JOptionPane.showMessageDialog(null,
                  Resources.getString(lafClassLoadError.id, lafClassLoadError.exception.getMessage()),
                  Resources.getString("Startup.laf_default"),
                  JOptionPane.WARNING_MESSAGE);
        }
      });
    }
    catch (InterruptedException | InvocationTargetException e) {
      ErrorDialog.bug(e);
    }
  }

  /**
   * Install and set the look and feel.
   * @param defaultLaf The name of the class that implements this look and feel.
   * @param lookAndFeelPath The name of the (jar) file containing the look and feel class.
   *                        This can be absolute or relative path.
   * @return Return null on success otherwise returns an error object.
   */
  protected LafLoadError installLookAndFeel(String defaultLaf, String lookAndFeelPath) {
    LafLoadError loadError = null;
    Path filePath = Paths.get(lookAndFeelPath);
    if (filePath.getRoot() == null) {
      // Not an absolute path. Prepend the user dir.
      final Path root = Paths.get(String.valueOf(Info.getBaseDir()));
      filePath = root.resolve(filePath);
    }
    final URI u = filePath.toUri();

    try {
      // Create an instance of URLClassloader using the URL.
      final ClassLoader loader = URLClassLoader.newInstance(new URL[]{u.toURL()}, getClass().getClassLoader());
      final Class<?> lookAndFeelClass = Class.forName(defaultLaf, true, loader);
      final LookAndFeel lookAndFeel = (LookAndFeel) lookAndFeelClass.getConstructors()[0].newInstance();

      UIManager.setLookAndFeel(lookAndFeel);
      UIManager.installLookAndFeel(lookAndFeel.getName(), lookAndFeelClass.getName());
      return null; // Success

    }
    catch (ClassNotFoundException | InstantiationException | IllegalAccessException | InvocationTargetException ex) {
      loadError = new LafLoadError("Startup.laf_not_found", ex);
    }
    catch (UnsupportedLookAndFeelException ex) {
      loadError = new LafLoadError("Startup.laf_unsupported", ex);
    }
    catch (MalformedURLException ex) {
      loadError = new LafLoadError("Startup.laf_malformed", ex);
    }
    return loadError;
  }

  protected void initSystemSpecificProperties() {}

  public void startErrorLog() {
    // begin the error log
    logger.info("Starting"); //$NON-NLS-1$
    logger.info("OS " + System.getProperty("os.name") + " " //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                      + System.getProperty("os.version") + " " //$NON-NLS-1$ //$NON-NLS-2$
                      + System.getProperty("os.arch")); //$NON-NLS-1$
    logger.info("Java version " + System.getProperty("java.version")); //$NON-NLS-1$ //$NON-NLS-2$
    logger.info("Java home " + System.getProperty("java.home")); //$NON-NLS-1$ //$NON-NLS-2$
    logger.info("VASSAL version " + Info.getVersion()); //$NON-NLS-1$
  }
}
