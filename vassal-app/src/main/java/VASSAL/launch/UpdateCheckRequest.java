/*
 *
 * Copyright (c) 2008 by Joel Uckelman
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

import java.util.concurrent.ExecutionException;

import javax.swing.JOptionPane;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.configure.LongConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.version.AbstractUpdateCheckRequest;

/**
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class UpdateCheckRequest extends AbstractUpdateCheckRequest {

  private static final Logger logger =
    LoggerFactory.getLogger(UpdateCheckRequest.class);

  private static final String NEXT_VERSION_CHECK = "nextVersionCheck"; //NON-NLS

  @Override
  protected void done() {
    try {
      final Boolean update = get();
      if (update) {
        // running version is obsolete
        if (JOptionPane.showConfirmDialog(
            ModuleManagerWindow.getInstance(),
            Resources.getString("UpdateCheckAction.update_available_message"),
            Resources.getString("UpdateCheckAction.update_available_title"),
            JOptionPane.YES_NO_OPTION,
            JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
          BrowserSupport.openURL(
              "https://github.com/vassalengine/vassal/releases"); //NON-NLS
        }
      }
    }
    catch (InterruptedException | ExecutionException e) {
      logger.error("", e);
    }
  }

  public static void updateCheck(Prefs globalPrefs) {
    // determine when we should next check on the current version of VASSAL
    final LongConfigurer nextVersionCheckConfig =
      new LongConfigurer(NEXT_VERSION_CHECK, null, -1L);
    globalPrefs.addOption(null, nextVersionCheckConfig);

    long nextVersionCheck = nextVersionCheckConfig.getLongValue(-1L);
    if (nextVersionCheck < System.currentTimeMillis()) {
      new UpdateCheckRequest().execute();
    }

    // set the time for the next version check
    if (nextVersionCheck == -1L) {
      // this was our first check; randomly check after 0-10 days to
      // to spread version checks evenly over a 10-day period
      nextVersionCheck = System.currentTimeMillis() +
                         (long) (Math.random() * 10 * 86400000);
    }
    else {
      // check again in 10 days
      nextVersionCheck += 10 * 86400000;
    }

    nextVersionCheckConfig.setValue(nextVersionCheck);
  }
}
