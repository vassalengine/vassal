/*
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Joel Uckelman, Brent Easton
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
import java.io.File;
import java.io.IOException;

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenuBar;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.build.module.ModuleExtension;
import VASSAL.build.module.WizardSupport;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.ModuleMetaData;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.JarArchive;
import VASSAL.tools.UsernameAndPasswordDialog;
import VASSAL.tools.menu.MacOSXMenuManager;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.version.VersionUtils;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class Player extends Launcher {
  public static void main(String[] args) throws IOException {
    Info.setConfig(new StandardConfig());
    new Player(args);
  }

  protected Player(String[] args) {
    // the ctor is protected to enforce that it's called via main()
    super(args);
  }

  @Override
  protected MenuManager createMenuManager() {
    return SystemUtils.IS_OS_MAC ?
      new MacOSXMenuManager() : new PlayerMenuManager();
  }

  @Override
  protected void launch() throws IOException {
    if (lr.builtInModule) {
      GameModule.init(createModule(createDataArchive()));

      if (lr.autoext != null) {
        for (final String ext : lr.autoext) {
          createExtension(ext).build();
        }
      }

      createExtensionsLoader().addTo(GameModule.getGameModule());
      Localization.getInstance().translate();
      showWizardOrPlayerWindow(GameModule.getGameModule());
    }
    else {
      GameModule.init(createModule(createDataArchive()));
      createExtensionsLoader().addTo(GameModule.getGameModule());
      Localization.getInstance().translate();
      final GameModule m = GameModule.getGameModule();
      if (lr.game != null) {
        m.getPlayerWindow().setVisible(true);
        m.setGameFile(lr.game.getName(), GameModule.GameFileMode.LOADED_GAME);
        m.getGameState().loadGameInBackground(lr.game);
      }
      else {
        showWizardOrPlayerWindow(m);
      }
    }
  }

  protected ExtensionsLoader createExtensionsLoader() {
    return new ExtensionsLoader();
  }

  protected ModuleExtension createExtension(String name) {
    return new ModuleExtension(new JarArchive(name));
  }

  protected DataArchive createDataArchive() throws IOException {
    if (lr.builtInModule) {
      return new JarArchive();
    }
    else {
      return new DataArchive(lr.module.getPath());
    }
  }

  protected GameModule createModule(DataArchive archive) {
    return new GameModule(archive);
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

  private void showWizardOrPlayerWindow(GameModule module) {
    final Boolean showWizard = (Boolean) Prefs.getGlobalPrefs().getValue(WizardSupport.WELCOME_WIZARD_KEY);
    if (Boolean.TRUE.equals(showWizard)) {
      module.getWizardSupport().showWelcomeWizard();
    }
    else {
      module.getPlayerWindow().setVisible(true);

      // prompt for username and password if wizard is off
      // but no username is set
      if (!isRealName()) {
        new UsernameAndPasswordDialog(module.getPlayerWindow()).setVisible(true);
      }
    }
  }

  public static class LaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public LaunchAction(ModuleManagerWindow mm, File module) {
      super(Resources.getString("Main.play_module_specific"), mm,
        Player.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.LOAD, module)
      );
      setEnabled(!isEditing(module));
    }

    public LaunchAction(ModuleManagerWindow mm, File module, File saveGame) {
      super(Resources.getString("General.open"), mm, Player.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.LOAD, module, saveGame)
      );
      setEnabled(!isEditing(module));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      if (isEditing(lr.module)) return;

      // don't permit loading of VASL saved before 3.4
      final AbstractMetaData data = MetaDataFactory.buildMetaData(lr.module);
      if (data instanceof ModuleMetaData) {
        final ModuleMetaData md = (ModuleMetaData) data;
        if (VersionUtils.compareVersions(md.getVassalVersion(), "3.4") < 0) {
          if ("VASL".equals(md.getName())) { //NON-NLS
            ErrorDialog.show(
              "Error.VASL_too_old", //NON-NLS
              Info.getVersion()
            );
            return;
          }
          else if ("VSQL".equals(md.getName())) { //NON-NLS
            ErrorDialog.show(
              "Error.VSQL_too_old", //NON-NLS
              Info.getVersion()
            );
            return;
          }
        }
      }
      else {
        // A module in the MM should be a valid Module, but people can and do delete
        // or replace module files while the MM is running.
        ErrorDialog.show("Error.invalid_vassal_module", lr.module.getAbsolutePath()); //NON-NLS
        lr.module = null;
        return;
      }

      // increase the using count
      incrementUsed(lr.module);
      super.actionPerformed(e);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();

          // reduce the using count
          decrementUsed(lr.module);
        }
      };
    }
  }

  public static class PromptLaunchAction extends LaunchAction {
    private static final long serialVersionUID = 1L;

    public PromptLaunchAction(ModuleManagerWindow mm) {
      super(mm, null);
      putValue(Action.NAME, Resources.getString("Main.play_module"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // prompt the user to pick a module
      if (promptForFile() == null) return;

      final AbstractMetaData data = MetaDataFactory.buildMetaData(lr.module);
      if (data != null && Info.isModuleTooNew(data.getVassalVersion())) {
        ErrorDialog.show(
          "Error.module_too_new", //NON-NLS
          lr.module.getPath(),
          data.getVassalVersion(),
          Info.getVersion()
        );
        return;
      }

      super.actionPerformed(e);
    }
  }

  private static class PlayerMenuManager extends MenuManager {
    private final MenuBarProxy menuBar = new MenuBarProxy();

    @Override
    public JMenuBar getMenuBarFor(JFrame fc) {
      return (fc instanceof PlayerWindow) ? menuBar.createPeer() : null;
    }

    @Override
    public MenuBarProxy getMenuBarProxyFor(JFrame fc) {
      return (fc instanceof PlayerWindow) ? menuBar : null;
    }
  }
}
