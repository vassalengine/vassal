/*
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.launch;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Map;
import java.util.zip.ZipFile;

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.build.module.GlobalOptions;
import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.ui.ShowServerStatusAction;
import VASSAL.configure.ShowHelpAction;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.WrapLayout;

public class PlayerWindow extends JFrame {
  private static final long serialVersionUID = 1L;

  protected static final PlayerWindow instance = new PlayerWindow();

  public static PlayerWindow getInstance() {
    return instance;
  }

  public JToolBar getToolBar() {
    return toolBar;
  }

  public JPanel getControlPanel() {
    return controlPanel;
  }

  private final Map<MenuKey,JMenuItem> menuItems =
    new HashMap<MenuKey,JMenuItem>();

  protected final JMenuBar menuBar = new JMenuBar();

  public JMenu getFileMenu() {
    return fileMenu;
  }

  public JMenu getToolsMenu() {
    return toolsMenu;
  }

  public JMenu getHelpMenu() {
    return helpMenu;
  }

  protected final JMenu fileMenu;
  protected final JMenu toolsMenu;
  protected final JMenu helpMenu;
 
  public enum MenuKey {
    OPEN_MODULE,
    CLOSE_MODULE,
    EDIT_MODULE,
    NEW_GAME,
    OPEN_GAME,
    SAVE_GAME,
    SAVE_GAME_AS,
    CLOSE_GAME,
    NEW_LOG,
    END_LOG,
    QUIT,
    SERVER_STATUS,
    EDIT_PREFS,
    HELP,
    ABOUT_VASSAL 
  };
 
  public JMenuItem getMenuItem(MenuKey key) {
    return menuItems.get(key);
  }

  private int findMenuItem(JMenu menu, JMenuItem item) {
    for (int i = 0; i < menu.getItemCount(); i++) {
      if (item == menu.getItem(i)) return i;
    }
    return -1;
  }

  public JMenuItem setMenuItem(MenuKey key, Action action) {
    final JMenuItem oldItem = getMenuItem(key);
    for (int i = 0; i < menuBar.getMenuCount(); i++) {
      final JMenu menu = menuBar.getMenu(i);
      final int pos = findMenuItem(menu, oldItem);
      if (pos != -1) {
        menu.remove(pos);
        return menuItems.put(key, menu.insert(action, pos));
      }
    }
    return null;
  }

  public JMenuItem setMenuItem(MenuKey key, JMenuItem item) {
    final JMenuItem oldItem = getMenuItem(key);
    for (int i = 0; i < menuBar.getMenuCount(); i++) {
      final JMenu menu = menuBar.getMenu(i);
      final int pos = findMenuItem(menu, oldItem);
      if (pos != -1) {
        menu.remove(pos);
        return menuItems.put(key, menu.insert(item, pos));
      }
    }
    return null;
  }

  protected final JToolBar toolBar = new JToolBar();
  protected final JPanel controlPanel = new JPanel();

  protected PlayerWindow() {
    setTitle("VASSAL");

    setLayout(new BorderLayout());
    setJMenuBar(menuBar);

    // build File menu
    fileMenu = new JMenu(Resources.getString("General.file"));
    fileMenu.setMnemonic(KeyEvent.VK_F);
    menuBar.add(fileMenu);

    final Runnable translateModule = new Runnable() {
      public void run() {
        try {
          Localization.getInstance().translate();
        }
        catch (IOException e) {
// FIXME: raise error dialog
          e.printStackTrace();
        }
      }
    };

    final Runnable loadExtensions = new Runnable() {
      public void run() {
        new ExtensionsLoader().addTo(GameModule.getGameModule());
      }
    }; 

    final Runnable showWelcomeWizard = new Runnable() {
      public void run() {
        if (Boolean.TRUE.equals(
              Prefs.getGlobalPrefs().getValue(GlobalOptions.WELCOME_WIZARD)))
        GameModule.getGameModule().getWizardSupport().showWelcomeWizard();
      }
    };

    final LoadModuleAction loadModuleAction =
      new LoadModuleAction(controlPanel);
    loadModuleAction.addAction(translateModule);
    loadModuleAction.addAction(loadExtensions);
    loadModuleAction.addAction(showWelcomeWizard);

    final Runnable toggleMenuItems = new Runnable() {
      public void run() {
        loadModuleAction.setEnabled(false);
//        closeModule.setEnabled(true);

        if (EditorWindow.hasInstance())
          ModuleEditorWindow.getInstance()
                            .moduleLoading(GameModule.getGameModule());
      }
    };

    loadModuleAction.addAction(toggleMenuItems);

    final JMenuItem closeModule = new JMenuItem("Close Module");
    closeModule.setEnabled(false);

    menuItems.put(MenuKey.OPEN_MODULE, fileMenu.add(loadModuleAction));
    menuItems.put(MenuKey.CLOSE_MODULE, fileMenu.add(closeModule));

    fileMenu.addSeparator();

    final JMenuItem newGame =
      new JMenuItem(Resources.getString("GameState.new_game"));
    newGame.setMnemonic('N');
    newGame.setEnabled(false);

    final JMenuItem openGame =
      new JMenuItem(Resources.getString("GameState.load_game"));
    openGame.setMnemonic('L');
    openGame.setEnabled(false);

    final JMenuItem saveGame =
      new JMenuItem(Resources.getString("GameState.save_game"));
    saveGame.setMnemonic('S');
    saveGame.setEnabled(false);

    final JMenuItem closeGame =
      new JMenuItem(Resources.getString("GameState.close_game"));
    closeGame.setMnemonic('C');
    closeGame.setEnabled(false);

    menuItems.put(MenuKey.NEW_GAME, fileMenu.add(newGame));
    menuItems.put(MenuKey.OPEN_GAME, fileMenu.add(openGame));
    menuItems.put(MenuKey.SAVE_GAME, fileMenu.add(saveGame));
    menuItems.put(MenuKey.CLOSE_GAME, fileMenu.add(closeGame));

    fileMenu.addSeparator();

    final JMenuItem newLog =
      new JMenuItem(Resources.getString("BasicLogger.begin_logfile"));
    newLog.setMnemonic('B');
    newLog.setEnabled(false);
    
    final JMenuItem endLog =
      new JMenuItem(Resources.getString("BasicLogger.end_logfile"));
    endLog.setMnemonic('E');
    endLog.setEnabled(false);

    menuItems.put(MenuKey.NEW_LOG, fileMenu.add(newLog));
    menuItems.put(MenuKey.END_LOG, fileMenu.add(endLog));

    fileMenu.addSeparator();

    final JMenuItem quit = new JMenuItem(Resources.getString(Resources.QUIT));
    quit.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.exit(0);
      }
    });

    quit.setMnemonic('Q');
    menuItems.put(MenuKey.QUIT, fileMenu.add(quit));
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    // build Tools menu
    toolsMenu = new JMenu(Resources.getString("General.tools"));
    toolsMenu.setMnemonic(KeyEvent.VK_T);  
    menuBar.add(toolsMenu);

    menuItems.put(MenuKey.SERVER_STATUS, toolsMenu.add(
      new ShowServerStatusAction(new CgiServerStatus(), null)));

    final JMenuItem editModule = new JMenuItem(
      Resources.getString("PlayerWindow.module_editor"));
    editModule.setEnabled(true);
    editModule.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        final GameModule mod = GameModule.getGameModule();
        if (mod != null) {
          ModuleEditorWindow.getInstance().moduleLoading(mod);
          mod.setDataArchive(new ArchiveWriter(mod.getDataArchive().getArchive()));
        }
        ModuleEditorWindow.getInstance().setVisible(true);
      }
    });

    menuItems.put(MenuKey.EDIT_MODULE, toolsMenu.add(editModule));

    final JMenuItem editPrefs =
      new JMenuItem(Resources.getString("Prefs.edit_preferences"));
    editPrefs.setEnabled(false);
    
    menuItems.put(MenuKey.EDIT_PREFS, toolsMenu.add(editPrefs));

    // build Help menu
    helpMenu = new JMenu(Resources.getString("General.help"));
/*
   {
      public JMenuItem add(Action a) {
        for (int i = 0; i < getItemCount(); i++) {
          if (getMenuComponent(i) instanceof JSeparator) {
            return insert(a, i);
          }
        }
        return super.add(a);
      }

      public JMenuItem add(JMenuItem item) {
        for (int i = 0; i < getItemCount(); i++) {
          if (getMenuComponent(i) instanceof JSeparator) {
            return insert(item, i);
          }
        }
        return super.add(item);
      }
    };
*/

    helpMenu.setMnemonic(KeyEvent.VK_H);  
    menuBar.add(helpMenu);

    try {
      final File readme =
        new File(Documentation.getDocumentationBaseDir(),"README.html");
      menuItems.put(MenuKey.HELP,
        helpMenu.add(new ShowHelpAction(readme.toURI().toURL(), null)));
    }
    catch (MalformedURLException e) {
// FIXME: raise error dialog
      e.printStackTrace();
    }

    helpMenu.addSeparator();

    final Action aboutVASSAL = AboutVASSAL.getAction();
    menuItems.put(MenuKey.ABOUT_VASSAL, helpMenu.add(aboutVASSAL));

    // build toolbar
    toolBar.setLayout(new WrapLayout(FlowLayout.LEFT, 0, 0));
    toolBar.setAlignmentX(0.0f);
    toolBar.setFloatable(false);
    add(toolBar, BorderLayout.NORTH);   
    
    // build central area
    controlPanel.setLayout(new BorderLayout());
    controlPanel.setPreferredSize(new Dimension(800,600));
    add(controlPanel, BorderLayout.CENTER);
    
    pack(); 
  }

  public static void main(String[] args) {
    new PlayerWindow().setVisible(true);
  } 
}
