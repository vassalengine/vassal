/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman 
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
import java.net.ServerSocket;
import java.util.List;

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenuBar;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.build.module.ModuleExtension;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.JarArchive;
import VASSAL.tools.menu.MacOSXMenuManager;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class Player extends Launcher {
  public static void main(String[] args) {
    new Player(args);
  }

  protected Player(String[] args) {
    // the ctor is protected to enforce that it's called via main()
    super(args);
  }

  protected MenuManager createMenuManager() {
    return Info.isMacOSX() ? new MacOSXMenuManager() : new PlayerMenuManager();
  }

  protected CommandServer createCommandServer(ServerSocket serverSocket) {
    return new PlayerCommandServer(serverSocket);
  }

  protected static class PlayerCommandServer extends CommandServer {
    public PlayerCommandServer(ServerSocket serverSocket) {
      super(serverSocket);
    }

    @Override
    protected Object reply(Object cmd) {
      if ("REQUEST_CLOSE".equals(cmd)) {
        final GameModule module = GameModule.getGameModule();
        module.getFrame().toFront();
        final boolean shutDown = module.shutDown();
        try {
          return shutDown ? "OK" : "NOK";
        }
        finally {
          if (shutDown) System.exit(0);
        }
      }
      else {
        return "UNRECOGNIZED_COMMAND";
      } 
    }
  }

  protected void launch() throws IOException {
    try {
      if (lr.builtInModule) {
        GameModule.init(createModule(createDataArchive()));
        for (String ext : lr.autoext) {
          createExtension(ext).build();
        }
        createExtensionsLoader().addTo(GameModule.getGameModule());
        Localization.getInstance().translate();
        GameModule.getGameModule().getWizardSupport().showWelcomeWizard();
      }
      else {
        GameModule.init(createModule(createDataArchive()));
        createExtensionsLoader().addTo(GameModule.getGameModule());
        Localization.getInstance().translate();
        final GameModule m = GameModule.getGameModule();
        if (lr.game != null) {
          m.getFrame().setVisible(true);
          m.getGameState().loadGameInBackground(lr.game);
        }
        else {
          m.getWizardSupport().showWelcomeWizard();
        }
      }

      if (cmdC != null) cmdC.request("NOTIFY_OPEN_OK");
    }
    catch (IOException e) {
      if (cmdC != null) cmdC.request("NOTIFY_OPEN_FAILED");
      throw e;
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
    return new BasicModule(archive);
  }

  public static class LaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public LaunchAction(ModuleManagerWindow mm, File module) {
      super(Resources.getString("Main.play_module_specific"), mm,
        Player.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.LOAD, module)
      );
      setEnabled(!editing.contains(module));
    }
    
    public LaunchAction(ModuleManagerWindow mm, File module, File saveGame) {
      super(Resources.getString("General.open"), mm, Player.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.LOAD, module, saveGame)
      );
      setEnabled(!editing.contains(module));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // register that this module is being used
      if (editing.contains(lr.module)) return;
      Integer count = using.get(lr.module);
      using.put(lr.module, count == null ? 1 : ++count);

      super.actionPerformed(e);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();

          // reduce the using count
          Integer count = using.get(lr.module);
          if (count == 1) using.remove(lr.module);
          else using.put(lr.module, --count);
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
