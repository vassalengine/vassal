/*
 * $Id$
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

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.logging.Logger;
import VASSAL.tools.version.AbstractUpdateCheckRequest;
import VASSAL.tools.version.VassalVersion;

/**
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class UpdateCheckRequest extends AbstractUpdateCheckRequest {
  @Override
  protected void done() {
    try {
      final VassalVersion update = get();
      if (update != null) {
        // running version is obsolete
        if (JOptionPane.showConfirmDialog(
            ModuleManagerWindow.getInstance(),
            Resources.getString("UpdateCheckAction.update_available_message"),
            Resources.getString("UpdateCheckAction.update_available_title"),
            JOptionPane.YES_NO_OPTION,
            JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION) {
          BrowserSupport.openURL(
              "https://sourceforge.net/project/showfiles.php?group_id=90612");
        }
      }
    }
    catch (InterruptedException e) {
      Logger.log(e);
    }
    catch (ExecutionException e) {
      Logger.log(e);
    }
  }
}
