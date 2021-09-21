/*
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
import java.net.URL;

import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

import org.apache.commons.lang3.SystemUtils;

/**
 * Utility class for displaying an external browser window.
 *
 * @author rkinney
 */
public class BrowserSupport {
  public static void openURI(URI uri) {
    //
    // This method is irritatingly complex because java.awt.Desktop seems
    // not to work sometimes on Windows, and sometimes blocks until the
    // program is closed (!) on Linux.
    //

    if (!SystemUtils.IS_OS_LINUX) {
      if (Desktop.isDesktopSupported()) {
        final Desktop desktop = Desktop.getDesktop();
        if (desktop.isSupported(Desktop.Action.BROWSE)) {
          try {
            desktop.browse(uri);
          }
          catch (IOException e) {
            ReadErrorDialog.error(e, uri.toString());
          }
          return;
        }
      }
    }

    // Try start, open, or xdg-open in case nothing else works
    final String uristr = uri.toString();

    String launcher;
    if (SystemUtils.IS_OS_WINDOWS) {
      launcher = "start";  //NON-NLS
    }
    else if (SystemUtils.IS_OS_MAC) {
      launcher = "open";  //NON-NLS
    }
    else {
      launcher = "xdg-open";  //NON-NLS
    }

    final ProcessBuilder pb = new ProcessBuilder(launcher, uristr);
    pb.redirectError(ProcessBuilder.Redirect.INHERIT);
    try {
      pb.start();
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, uristr);
    }
  }

  public static void openURL(URL url) {
    URI uri;
    try {
      try {
        uri = url.toURI();
      }
      catch (URISyntaxException e) {
        throw new IOException(e);
      }
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, url.toString());
      return;
    }

    openURI(uri);
  }

  public static void openURL(String url) {
    URI uri;
    try {
      try {
        uri = new URI(url);
      }
      catch (URISyntaxException e) {
        throw new IOException(e);
      }
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, url);
      return;
    }

    openURI(uri);
  }

  private static final HyperlinkListener listener = e -> {
    if (e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) {
      openURL(e.getURL());
    }
  };

  public static HyperlinkListener getListener() {
    return listener;
  }
}
