/*
 * $Id$
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

package VASSAL.launch;

import java.awt.event.ActionEvent;
import java.io.IOException;

import javax.swing.AbstractAction;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.io.IOUtils;

public class ShutDownAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  public ShutDownAction() {
    super(Resources.getString(Resources.QUIT));
  }

  public void actionPerformed(ActionEvent e) {
    if (GameModule.getGameModule() == null) {
      Prefs p = null;
      try {
        p = Prefs.getGlobalPrefs();
        p.write();
        p.close();
      }
      catch (IOException ex) {
        WriteErrorDialog.error(ex, Prefs.getGlobalPrefs().getFile().getPath());
      }
      finally {
        IOUtils.closeQuietly(p);
      }

      System.exit(0);
    }
    else if (GameModule.getGameModule().shutDown()) {
      System.exit(0);
    }
  }
}
