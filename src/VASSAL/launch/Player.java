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
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.Properties;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.build.module.ModuleExtension;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.JarArchive;
import VASSAL.tools.menu.MacOSXMenuManager;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;

public class Player {
//  protected boolean isFirstTime;
  protected boolean builtInModule;
  protected File moduleFile;
  protected File savedGame;
  protected List<String> extractTargets = new ArrayList<String>();
  protected List<String> autoExtensions = new ArrayList<String>();

  public Player(final String[] args) {
    StartUp.initSystemProperties();
//    StartUp.setupErrorLog();
    StartUp.startErrorLog();

    Thread.setDefaultUncaughtExceptionHandler(new ErrorLog());
    
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        try {
          Player.this.configure(args);
          Player.this.extractResourcesAndLaunch(0);
        }
        catch (IOException e) {
          reportError(e);
        }
      }
    });
  }

  protected void extractResourcesAndLaunch(final int resourceIndex) throws IOException {
    if (resourceIndex >= extractTargets.size()) {
      launch();
    }
    else {
      final Properties props = new Properties();
      final InputStream in =
        Player.class.getResourceAsStream(extractTargets.get(resourceIndex));
      if (in != null) {
        try {
          props.load(in);
        }
        finally {
          try {
            in.close();
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
      }

      new ResourceExtracter(Prefs.getGlobalPrefs(), props, new Observer() {
        public void update(Observable o, Object arg) {
          try {
            extractResourcesAndLaunch(resourceIndex + 1);
          }
          catch (IOException e) {
            reportError(e);
          }
        }
      }).install();
    }
  }

  protected void reportError(Exception e) {
    e.printStackTrace();
    String msg = e.getMessage();
    if (msg == null) {
      msg = e.getClass().getSimpleName();
    }
    JOptionPane.showMessageDialog(null, msg, Resources.getString("ResourceExtracter.install_failed"), JOptionPane.ERROR_MESSAGE);
  }

  protected void launch() throws IOException {
    if (Info.isMacOSX()) new MacOSXMenuManager();
    else new PlayerMenuManager();

    try {
/*
      if (isFirstTime) {
        new FirstTimeDialog().setVisible(true);
      }
      else if (builtInModule) {
*/
      if (builtInModule) {
        GameModule.init(createModule(createDataArchive()));
        for (String ext : autoExtensions) {
          createExtension(ext).build();
        }
        createExtensionsLoader().addTo(GameModule.getGameModule());
        Localization.getInstance().translate();
        GameModule.getGameModule().getWizardSupport().showWelcomeWizard();
      }
      else if (moduleFile == null) {
        return;
      }
      else {
        GameModule.init(createModule(createDataArchive()));
        createExtensionsLoader().addTo(GameModule.getGameModule());
        Localization.getInstance().translate();
        final GameModule m = GameModule.getGameModule();
        if (savedGame != null) {
          m.getFrame().setVisible(true);
          m.getGameState().loadGameInBackground(savedGame);
        }
        else {
          m.getWizardSupport().showWelcomeWizard();
        }
      }
    }  
    finally {
      System.out.print("\n");
    }
  }

  protected ExtensionsLoader createExtensionsLoader() {
    return new ExtensionsLoader();
  }

  protected ModuleExtension createExtension(String name) {
    return new ModuleExtension(new JarArchive(name));
  }

  protected DataArchive createDataArchive() throws IOException {
    if (builtInModule) {
      return new JarArchive();
    }
    else {
      return new DataArchive(moduleFile.getPath());
    }
  }

  protected GameModule createModule(DataArchive archive) {
    return new BasicModule(archive);
  }

  protected void configure(final String[] args) {
/*
    File prefsFile = new File(Info.getHomeDir(), "Preferences");
    isFirstTime = !prefsFile.exists();
*/
    int n = -1;
    while (++n < args.length) {
      final String arg = args[n];
      if ("-auto".equals(arg)) {
        builtInModule = true;
      }
      else if ("-extract".equals(arg)) {
        extractTargets.add(args[++n]);
      }
      else if ("-autoextensions".equals(arg)) {
        for (String ext : args[++n].split(",")) {
          autoExtensions.add(ext.replace("_"," "));
        }
      }
      else if ("-load".equals(arg)) {
        savedGame = new File(args[++n]);
      }
      else if (!arg.startsWith("-")) {
        moduleFile = new File(arg);
      }
    }
  }

  public static class LaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public LaunchAction(ModuleManagerWindow mm, File module) {
      super(Resources.getString("Main.play_module"), mm,
            Player.class.getName(), new String[0], module);
      setEnabled(!editing.contains(module));
    }
    
    public LaunchAction(ModuleManagerWindow mm, File module, File saveGame) {
      super(Resources.getString("General.open"), mm, Player.class.getName(),
            new String[] {"-load",saveGame.getPath()}, module);
      setEnabled(!editing.contains(module));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // register that this module is being used
      if (editing.contains(module)) return;
      Integer count = using.get(module);
      using.put(module, count == null ? 1 : ++count);

      super.actionPerformed(e);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();

          // reduce the using count
          Integer count = using.get(mod);
          if (count == 1) using.remove(mod);
          else using.put(mod, --count);
        }

        @Override
        protected void process(List<Void> chunks) {
          super.process(chunks);
          ((ModuleManagerWindow) window).addModule(mod);
        }
      };
    }
  }

  public static class PromptLaunchAction extends LaunchAction {
    private static final long serialVersionUID = 1L;

    public PromptLaunchAction(ModuleManagerWindow mm) {
      super(mm, null);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // prompt the user to pick a module
      if (promptForModule() == null) return;

      super.actionPerformed(e);
      module = null;
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

  public static void main(String[] args) {
    new Player(args);
  }
}
