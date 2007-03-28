/*
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
package VASSAL.tools;

import java.net.URL;
import javax.swing.JOptionPane;
import VASSAL.build.GameModule;
import edu.stanford.ejalbert.BrowserLauncher;
import edu.stanford.ejalbert.exception.BrowserLaunchingInitializingException;
import edu.stanford.ejalbert.exception.UnsupportedOperatingSystemException;

/**
 * Utility class for displaying an external browser window
 * @author rkinney
 */
public class BrowserSupport {
  private static BrowserLauncher browserLauncher;
  private static Exception initializationError;
  
  public static void openURL(URL url) {
    if (!initialized()) {
      initialize();
    }
    if (initializationError == null) {
      browserLauncher.openURLinBrowser(url.toString());
    }
    else {
      String msg = "Unable to launch browser window\n";
      if (initializationError.getMessage() != null) {
        msg += initializationError.getMessage()+"\n";
      }
      msg += "Open your browser and point it to "+url;
      JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), msg);
    }

  }

  private static void initialize() {
    try {
      browserLauncher = new BrowserLauncher();
    }
    catch (BrowserLaunchingInitializingException e) {
      initializationError = e;
    }
    catch (UnsupportedOperatingSystemException e) {
      initializationError = e;
    }
  }

  private static boolean initialized() {
    return browserLauncher != null || initializationError != null;
  }

}
