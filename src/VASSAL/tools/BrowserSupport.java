/*
 * $Id$
 *
 * Copyright (c) 2000-2011 by Rodney Kinney, Joel Uckelman
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
package VASSAL.tools;

import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import edu.stanford.ejalbert.BrowserLauncher;
import edu.stanford.ejalbert.exception.BrowserLaunchingInitializingException;
import edu.stanford.ejalbert.exception.UnsupportedOperatingSystemException;

import org.apache.commons.lang.SystemUtils;

// FIXME: Remove BrowserLauncher when we move to Java 1.6+.

/**
 * Utility class for displaying an external browser window.
 *
 * @author rkinney
 */
public class BrowserSupport {

  private static final BrowserLauncher browserLauncher;

  static {
    BrowserLauncher bl = null;

    if (SystemUtils.IS_JAVA_1_5) {
      try {
        bl = new BrowserLauncher();
      }
      catch (BrowserLaunchingInitializingException e) {
        ErrorDialog.bug(e);
      }
      catch (UnsupportedOperatingSystemException e) {
        ErrorDialog.bug(e);
      }
    }
    
    browserLauncher = bl;
  }

  private static void openURLWithBrowserLauncher(String url) {
    if (browserLauncher != null) {
      browserLauncher.openURLinBrowser(url);
    }
    else {
      ErrorDialog.show("BrowserSupport.unable_to_launch", url);
    }
  }

  private static void openURLWithDesktop(String url) {
    final Desktop desktop = Desktop.getDesktop();
    if (desktop.isSupported(Desktop.Action.BROWSE)) {
      try {
        desktop.browse(new URI(url));
      }
      catch (IOException e) {
        ErrorDialog.bug(e);
      }
      catch (IllegalArgumentException e) {
        ErrorDialog.bug(e);
      }
      catch (URISyntaxException e) {
        ErrorDialog.bug(e);
      }
    }
  }

  public static void openURL(String url) {
    if (!SystemUtils.IS_JAVA_1_5) {
      if (Desktop.isDesktopSupported()) {
        openURLWithDesktop(url);
      }
      else {
        openURLWithBrowserLauncher(url);
      }
    }
    else {
      openURLWithBrowserLauncher(url);
    }
  }

  private static final HyperlinkListener listener = new HyperlinkListener() {
    public void hyperlinkUpdate(HyperlinkEvent e) {
      if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
        openURL(e.getURL().toString());
      }
    }
  };

  public static HyperlinkListener getListener() {
    return listener;
  }
}
