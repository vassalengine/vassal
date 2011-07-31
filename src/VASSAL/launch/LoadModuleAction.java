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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import VASSAL.build.GameModule;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;
import VASSAL.tools.filechooser.FileChooser;

/**
 * Loads a module in play mode
 *
 * @author rodneykinney
 */
public class LoadModuleAction extends GameModuleAction {
  private static final long serialVersionUID = 1L;
  private File moduleFile;
  protected FileChooser fc;

  public LoadModuleAction(Component comp) {
    super(Resources.getString("Main.play_module"), comp);
  }

  public LoadModuleAction(File moduleFile) {
    super(Resources.getString("Main.play_module"), null);
    this.moduleFile = moduleFile;
  }

  public void performAction(ActionEvent e) throws IOException {
    actionCancelled = true;
    File target = moduleFile;
    if (target == null) {
      if (fc == null) {
        fc = FileChooser.createFileChooser(comp,
          (DirectoryConfigurer)
            Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY));
      }

      if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
        File f = fc.getSelectedFile();
        if (f != null && f.exists()) {
          target = f;
        }
      }

      // bail out if still no target
      if (target == null) return;
    }

    loadModule(target);
    actionCancelled = false;
  }

  protected void loadModule(File f) throws IOException {
//    final JFrame frame = ModuleManagerWindow.getInstance();
//    frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
//
//    try {
    GameModule.init(new BasicModule(new DataArchive(f.getPath())));
//      ModuleManagerWindow.getInstance().addModule(f);
    Localization.getInstance().translate();
    new ExtensionsLoader().addTo(GameModule.getGameModule());
    GameModule.getGameModule().getWizardSupport().showWelcomeWizard();

//      frame.setVisible(false);
//    }
//    finally {
//      frame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
//    }
  }
}
