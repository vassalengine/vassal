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

import java.io.File;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;

import VASSAL.Info;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MacOSXMenuManager;

/**
 * Tracks recently-used modules and builds the main GUI window for 
 * interacting with modules.
 * 
 * @author rodneykinney
 * @since 3.1.0
 */
public class ModuleManager {
  public ModuleManager(final String[] args) {
    StartUp.initSystemProperties();
    StartUp.setupErrorLog();
    StartUp.startErrorLog();

    Thread.setDefaultUncaughtExceptionHandler(new ErrorLog());
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        launch();
      }
    });
  }

  protected void launch() {
    if (Info.isMacOSX()) new MacOSXMenuManager();
    else new ModuleManagerMenuManager();

    final File prefsFile = new File(Info.getHomeDir(), "Preferences");
    final boolean isFirstTime = !prefsFile.exists();

    final ModuleManagerWindow w = ModuleManagerWindow.getInstance();
    w.setVisible(true);
    if (isFirstTime) new FirstTimeDialog(w).setVisible(true);
  }

  private static class ModuleManagerMenuManager extends MenuManager {
    private final MenuBarProxy menuBar = new MenuBarProxy();

    @Override
    public JMenuBar getMenuBarFor(JFrame fc) {
      return (fc instanceof ModuleManagerWindow) ? menuBar.createPeer() : null;
    }

    @Override
    public MenuBarProxy getMenuBarProxyFor(JFrame fc) {
      return (fc instanceof ModuleManagerWindow) ? menuBar : null;
    }
  }

  public static void main(String[] args) {
    new ModuleManager(args);
  }
}
