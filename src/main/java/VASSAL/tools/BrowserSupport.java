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

import org.apache.commons.lang3.SystemUtils;

/**
 * Utility class for displaying an external browser window.
 *
 * @author rkinney
 */
public class BrowserSupport {
  public static void openURL(String url) {
    //
    // This method is irritatingly complex because
    // java.awt.Desktop seems not to work sometimes on Windows.
    //
    if (Desktop.isDesktopSupported()) {
      final Desktop desktop = Desktop.getDesktop();
      if (desktop.isSupported(Desktop.Action.BROWSE)) {
        try {
          desktop.browse(new URI(url));
          return;
        }
        catch (IOException e) {
          // We ignore this on Windows, because Desktop seems flaky
          if (!SystemUtils.IS_OS_WINDOWS) {
            ReadErrorDialog.error(e, url);
            return;
          }
        }
        catch (IllegalArgumentException | URISyntaxException e) {
          ErrorDialog.bug(e);
          return;
        }
      }
    }
  }

  private static final HyperlinkListener listener = new HyperlinkListener() {
    @Override
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
