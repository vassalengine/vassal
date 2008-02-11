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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Map;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.JWindow;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.ModuleUpdaterDialog;
import VASSAL.configure.SaveAction;
import VASSAL.configure.SaveAsAction;
import VASSAL.configure.SavedGameUpdaterDialog;
import VASSAL.configure.ShowHelpAction;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslateVassalWindow;
import VASSAL.tools.imports.ImportAction;

public class EditorWindow extends JFrame {
  protected static final EditorWindow instance = new EditorWindow();

  public static EditorWindow getInstance() {
    return instance;
  }

  protected final HelpWindow helpWindow = new HelpWindow(
    Resources.getString("Editor.ModuleEditor.reference_manual"), //$NON-NLS-1$
    null
  );

  protected ConfigureTree tree;

  public void moduleLoading(GameModule mod) {
    tree = new ConfigureTree(mod, helpWindow);
    scrollPane.setViewportView(tree);
    pack();
  }

  private final Map<MenuKey,JMenuItem> menuItems =
    new HashMap<MenuKey,JMenuItem>();

  protected final JMenuBar menuBar = new JMenuBar();
  
  protected final JToolBar toolBar = new JToolBar();

  protected final JMenu fileMenu;
//  protected final JMenu editMenu;
  protected final JMenu toolsMenu;
  protected final JMenu helpMenu;

  public JMenu getFileMenu() {
    return fileMenu;
  }

/*
  public JMenu getEditMenu() {
    return editMenu;
  }
*/

  public JMenu getToolsMenu() {
    return toolsMenu;
  }

  public JMenu getHelpMenu() {
    return helpMenu;
  }

  public enum MenuKey {
    NEW,
    OPEN,
    SAVE,
    SAVE_AS,
    CLOSE,
    IMPORT,
    NEW_EXTENSION,
    LOAD_EXTENSION,
    QUIT,
    CREATE_MODULE_UPDATER,
    UPDATE_SAVED,
    TRANSLATE_VASSAL,
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

  protected final JScrollPane scrollPane =
    new JScrollPane(new JPanel(), // prevents a NullPointerException on packing
                    JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                    JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

  protected EditorWindow() {
    setTitle("VASSAL Editor");

    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    setLayout(new BorderLayout());
    setJMenuBar(menuBar);
    
    toolBar.setFloatable(false);
    add(toolBar, BorderLayout.NORTH);

    // build File menu
    fileMenu = new JMenu(Resources.getString("General.file"));
    fileMenu.setMnemonic(KeyEvent.VK_F);
    menuBar.add(fileMenu);

    final CreateModuleAction createModuleAction = new CreateModuleAction(this);
    menuItems.put(MenuKey.NEW, fileMenu.add(createModuleAction));

    final EditModuleAction editModuleAction = new EditModuleAction(this);
    menuItems.put(MenuKey.OPEN, fileMenu.add(editModuleAction));

    final JMenuItem closeModule = new JMenuItem("Close Module");
    closeModule.setEnabled(false); 
    menuItems.put(MenuKey.CLOSE, fileMenu.add(closeModule));

    final SaveAction saveAction = new SaveAction() {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
//        ModuleEditWindow.this.save();
      }
    };

    saveAction.setEnabled(false);
    menuItems.put(MenuKey.SAVE, fileMenu.add(saveAction));
    toolBar.add(saveAction);

    final SaveAsAction saveAsAction = new SaveAsAction() {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
//        ModuleEditWindow.this.saveAs();
      }
    };

    saveAsAction.setEnabled(false);
    menuItems.put(MenuKey.SAVE_AS, fileMenu.add(saveAsAction));
    toolBar.add(saveAsAction);

    fileMenu.addSeparator(); 

    final ImportAction importAction = new ImportAction(this);
    menuItems.put(MenuKey.IMPORT, fileMenu.add(importAction));

    fileMenu.addSeparator();

    final NewExtensionAction newExtensionAction = new NewExtensionAction(this); 
    newExtensionAction.setEnabled(false);
    menuItems.put(MenuKey.NEW_EXTENSION, fileMenu.add(newExtensionAction));

    final EditExtensionAction editExtensionAction =
      new EditExtensionAction(this);
    editExtensionAction.setEnabled(false);      
    menuItems.put(MenuKey.LOAD_EXTENSION, fileMenu.add(editExtensionAction));

    fileMenu.addSeparator();

    final JMenuItem quit = new JMenuItem(Resources.getString(Resources.QUIT));
    quit.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.exit(0);
      }
    });
   
    quit.setMnemonic('Q');
    menuItems.put(MenuKey.QUIT, fileMenu.add(quit));

/*
    // build Edit menu
    editMenu = new JMenu(Resources.getString("General.edit"));
    menuBar.add(editMenu);
*/

    // build Tools menu
    toolsMenu = new JMenu(Resources.getString("General.tools"));
    menuBar.add(toolsMenu);
    
    final JMenuItem createModuleUpdater = toolsMenu.add(Resources.getString(
      "Editor.ModuleEditor.create_module_updater")); //$NON-NLS-1$
    createModuleUpdater.setEnabled(false);
    createModuleUpdater.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new ModuleUpdaterDialog(EditorWindow.this).setVisible(true);
      }
    });

    menuItems.put(MenuKey.CREATE_MODULE_UPDATER, createModuleUpdater);

    final JMenuItem updateSavedGame = toolsMenu.add(Resources.getString(
      "Editor.ModuleEditor.update_saved")); //$NON-NLS-1$
    updateSavedGame.setEnabled(false);
    updateSavedGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new SavedGameUpdaterDialog(EditorWindow.this).setVisible(true);
      }
    });
    
    menuItems.put(MenuKey.UPDATE_SAVED, updateSavedGame);
    
    toolsMenu.addSeparator();

    final JMenuItem translateVASSAL = toolsMenu.add(Resources.getString(
        "Editor.ModuleEditor.translate_vassal")); //$NON-NLS-1$
    translateVASSAL.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new TranslateVassalWindow(EditorWindow.this).setVisible(true);
      }
    });

    menuItems.put(MenuKey.TRANSLATE_VASSAL, translateVASSAL);

    final Runnable toggleItemsAfterModuleStart = new Runnable() {
      public void run() {
        createModuleAction.setEnabled(false);
        editModuleAction.setEnabled(false);
        // closeModule.setEnabled(true);
        saveAction.setEnabled(true);
        saveAsAction.setEnabled(true);
        importAction.setEnabled(false);
        newExtensionAction.setEnabled(true);
        editExtensionAction.setEnabled(true);
        createModuleUpdater.setEnabled(true);
        updateSavedGame.setEnabled(true);

        PlayerWindow.getInstance()
                    .getMenuItem(PlayerWindow.MenuKey.OPEN_MODULE)
                    .setEnabled(false);
      }
    };

    editModuleAction.addAction(toggleItemsAfterModuleStart);
    createModuleAction.addAction(toggleItemsAfterModuleStart);
 
    // build Help menu
    helpMenu = new JMenu(Resources.getString("General.help"));
    menuBar.add(helpMenu);

    Action helpAction = null;

    try {
      File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
      dir = new File(dir, "ReferenceManual"); //$NON-NLS-1$
      helpAction = new ShowHelpAction(HelpFile.toURL(new File(dir, "index.htm")), helpWindow.getClass().getResource("/images/Help16.gif")); //$NON-NLS-1$ //$NON-NLS-2$
      helpAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString("Editor.ModuleEditor.reference_manual")); //$NON-NLS-1$

      toolBar.add(helpAction);
      menuItems.put(MenuKey.HELP, helpMenu.add(helpAction));
    }
    catch (MalformedURLException e) {
      e.printStackTrace();
    }

    helpMenu.addSeparator();

    final Action aboutVASSAL = AboutVASSAL.getAction();
    menuItems.put(MenuKey.ABOUT_VASSAL, helpMenu.add(aboutVASSAL));

    add(scrollPane, BorderLayout.CENTER);
    pack();
  }

  public static void main(String[] args) {
    new EditorWindow().setVisible(true);
  }
}
