/*
 * Copyright (c) 2026 by The VASSAL Development Team
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

// For future improvements
// import org.slf4j.Logger;
// import org.slf4j.LoggerFactory;

import java.io.IOException;
import javax.swing.JFrame;
import javax.swing.JMenuBar;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.Info;
import VASSAL.library.GameLibraryWindow;
// import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.tools.menu.MacOSXMenuManager;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;


public class LibraryBrowser extends Launcher {
  // For future improvements
  // private static final Logger logger =
  //   LoggerFactory.getLogger(LibraryBrowser.class);


  LibraryBrowser(String[] args) {
    super(args);
  }


  @Override
  protected void launch() throws IOException {
    // Localization.getInstance().translate();

    final GameLibraryWindow window = new GameLibraryWindow();
    window.pack();
    window.setVisible(true);
  }
  
  public static class LaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;
    
    public LaunchAction(ModuleManagerWindow mm) {
      super(Resources.getString("Main.browse_library"), mm,
            LibraryBrowser.class.getName(),
            new LaunchRequest(LaunchRequest.Mode.LIBRARY_BROWSER));
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();
        }
      };
    }
  }

  public static class PromptLaunchAction extends LaunchAction {
    private static final long serialVersionUID = 1L;

    public PromptLaunchAction(ModuleManagerWindow mm) {
      super(mm);
      putValue(NAME, Resources.getString("Main.browse_library"));
    }
  }

  /**
   * The Menu bar manager
   */
  private static class LibraryBrowserMenuManager extends MenuManager {
    private final MenuBarProxy menuBar = new MenuBarProxy();

    @Override
    public JMenuBar getMenuBarFor(JFrame fc) {
      return (fc instanceof GameLibraryWindow) ? menuBar.createPeer() : null;
    }

    @Override
    public MenuBarProxy getMenuBarProxyFor(JFrame fc) {
      return (fc instanceof GameLibraryWindow) ? menuBar : null;
    }
  }
  

  /**
   * Create the manager of actions
   */
  @Override
  protected MenuManager createMenuManager() {
    return SystemUtils.IS_OS_MAC ?
      new MacOSXMenuManager() : new LibraryBrowserMenuManager();
  }
  
  public static void main(String[] args) throws Exception {
    Info.setConfig(new StandardConfig());

    new LibraryBrowser(args);
  }

  
}
