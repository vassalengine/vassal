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
package VASSAL.launch;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import VASSAL.build.GameModule;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.build.module.WizardSupport;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;
import VASSAL.tools.UsernameAndPasswordDialog;
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

  @Override
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
        final File f = fc.getSelectedFile();
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
    GameModule.init(new GameModule(new DataArchive(f.getPath())));
    Localization.getInstance().translate();
    final GameModule m = GameModule.getGameModule();
    new ExtensionsLoader().addTo(m);
    showWizardOrPlayerWindow(m);
  }

  private void showWizardOrPlayerWindow(GameModule module) {
    final Boolean showWizard = (Boolean) Prefs.getGlobalPrefs().getValue(WizardSupport.WELCOME_WIZARD_KEY);
    if (Boolean.TRUE.equals(showWizard)) {
      module.getWizardSupport().showWelcomeWizard();
    }
    else {
      module.getPlayerWindow().setVisible(true);

      // prompt for username and password if wizard is off
      // but no username is set, or password is blank
      if (!isRealName() || !isNonBlankPassword()) {
        new UsernameAndPasswordDialog(module.getPlayerWindow()).setVisible(true);
      }
    }
  }

  /**
   * Returns true if user has supplied a real name for current GameModule.
   *
   * Test's whether GameModule.REAL_NAME is non-empty and not "newbie"
   *
   * @return <code>true</code> if user supplied a real name
   */
  private boolean isRealName() {
    final String name = (String)GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME);
    return name != null && !name.isEmpty() && !name.equals(Resources.getString("Prefs.newbie"));
  }

  /**
   * Returns true if user has supplied a real password for current GameModule.
   *
   * Test's whether GameModule.SECRET_NAME is non-empty
   *
   * @return <code>true</code> if user supplied a real password
   */
  private boolean isNonBlankPassword() {
    final String pwd = (String)GameModule.getGameModule().getPrefs().getValue(GameModule.SECRET_NAME);
    return (pwd != null) && !pwd.isEmpty();
  }
}
