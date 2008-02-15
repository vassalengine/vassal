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
import java.awt.Toolkit;
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
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.ModuleExtension;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.ModuleUpdaterDialog;
import VASSAL.configure.SaveAction;
import VASSAL.configure.SaveAsAction;
import VASSAL.configure.ShowHelpAction;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidationReportDialog;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslateVassalWindow;

/**
 * EditorWindow is the base class for the three top-level component
 * editors :- ModuleEditorWindow, ExtensionEditorWindow, PluginEditorWindow
 * 
 * @author Brent Easton
 *
 */
public abstract class EditorWindow extends JFrame {
  private static final long serialVersionUID = 1L;
  protected static EditorWindow instance = null;
  protected SaveAction saveAction;
  protected SaveAsAction saveAsAction;
  protected JMenuItem componentHelpItem;
  protected JMenuItem createUpdater;
  protected JMenuItem close;

  public static boolean hasInstance() {
    return instance != null;
  }

  protected final HelpWindow helpWindow = new HelpWindow(
    Resources.getString("Editor.ModuleEditor.reference_manual"), //$NON-NLS-1$
    null
  );

  protected ConfigureTree tree;
  
  public abstract String getEditorType();
  public abstract void moduleLoading(GameModule mod);
  public abstract void moduleLoading(GameModule mod, ModuleExtension ext);

  private final Map<MenuKey,JMenuItem> menuItems =
    new HashMap<MenuKey,JMenuItem>();

  protected final JMenuBar menuBar = new JMenuBar();
  
  protected final JToolBar toolBar = new JToolBar();

  private final JMenu fileMenu;
  private final JMenu editMenu;
  private final JMenu toolsMenu;
  private final JMenu helpMenu;

  public JMenu getFileMenu() {
    return fileMenu;
  }

  public JMenu getEditMenu() {
    return editMenu;
  }

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
 
  protected void addMenuItem(MenuKey key, JMenuItem item) {
    menuItems.put(key, item);
  }
  
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

  protected final JScrollPane scrollPane;
  
  protected EditorWindow() {
    setTitle("VASSAL " + getEditorType() + " Editor");    
    setLayout(new BorderLayout());
    setJMenuBar(menuBar);
    
    toolBar.setFloatable(false);
    add(toolBar, BorderLayout.NORTH);

    // build File menu
    fileMenu = new JMenu(Resources.getString("General.file"));
    fileMenu.setMnemonic(KeyEvent.VK_F);
    menuBar.add(fileMenu);
    populateFileMenu(fileMenu);

    // Create an empty Edit Menu. It is populated by the edit Tree
    // after the component is loaded
    editMenu = new JMenu(Resources.getString("General.edit"));
    menuBar.add(editMenu);

    // build Tools menu
    toolsMenu = new JMenu(Resources.getString("General.tools"));
    menuBar.add(toolsMenu);
    populateToolsMenu(toolsMenu);
 
    // build Help menu
    helpMenu = new JMenu(Resources.getString("General.help"));
    menuBar.add(helpMenu);
    populateHelpMenu(helpMenu);
 
    // turn off File > Edit Module in PlayerWindow when the Editor is created
    PlayerWindow.getInstance()
                .getMenuItem(PlayerWindow.MenuKey.EDIT_MODULE)
                .setEnabled(false);
    
    // the presence of the panel prevents a NullPointerException on packing
    final JPanel panel = new JPanel();
    panel.setPreferredSize(new Dimension(250,400));

    scrollPane = new JScrollPane(
      panel,
      JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
      JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    
    add(scrollPane, BorderLayout.CENTER);
    pack();
  }

  /**
   * Add options to the File Menu. Different component types will 
   * add different options.
   */
  protected abstract void populateFileMenu(JMenu menu);

  /**
   * Add options to the Tools Menu. Different component types will 
   * add different options.
   */
  protected abstract void populateToolsMenu(JMenu menu);
 
  /**
   * Add options to the Help Menu. All component types share the same Help 
   * Menu options.
   */
  protected void populateHelpMenu(JMenu menu) {

    Action helpAction = null;

    try {
      File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
      dir = new File(dir, "ReferenceManual"); //$NON-NLS-1$
      helpAction = new ShowHelpAction(
        HelpFile.toURL(new File(dir, "index.htm")), //$NON-NLS-1$
        helpWindow.getClass().getResource("/images/Help16.gif")); //$NON-NLS-1$
      helpAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString(
        "Editor.ModuleEditor.reference_manual")); //$NON-NLS-1$

      toolBar.add(helpAction);
      menuItems.put(MenuKey.HELP, menu.add(helpAction));
    }
    catch (MalformedURLException e) {
      e.printStackTrace();
    }

    // Temporary Component Help item until the Module is loaded
    componentHelpItem = new JMenuItem("Component help");
    componentHelpItem.setEnabled(false);
    menu.add(componentHelpItem);
    
    menu.addSeparator();

    final Action aboutVASSAL = AboutVASSAL.getAction();
    menuItems.put(MenuKey.ABOUT_VASSAL, menu.add(aboutVASSAL));
    
  }
  
  /*
   * Menu Items common to multiple components
   */  
  protected void addSaveMenuItem(JMenu menu) {
    saveAction = new SaveAction() {
      private static final long serialVersionUID = 1L;
      public void actionPerformed(ActionEvent e) {
        save();
        treeStateChanged(false);
      }
    };

    saveAction.setEnabled(false);
    saveAction.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_S, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    addMenuItem(MenuKey.SAVE, menu.add(saveAction));
    toolBar.add(saveAction);
  }
  
  protected void addSaveAsMenuItem(JMenu menu) {
    saveAsAction = new SaveAsAction() {
      private static final long serialVersionUID = 1L;
      public void actionPerformed(ActionEvent e) {
        saveAs();
        treeStateChanged(false);
      }
    };

    saveAsAction.setEnabled(false);
    saveAsAction.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_A, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
    addMenuItem(MenuKey.SAVE_AS, menu.add(saveAsAction));
    toolBar.add(saveAsAction);
  }
  
  protected void addCloseMenuItem(JMenu menu) {
    close = new JMenuItem("Close " + getEditorType());
    close.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        close();
      }
    });
    close.setEnabled(false);
    addMenuItem(MenuKey.CLOSE, menu.add(close));
  }
  
  /*
   * Each component must Save, SaveAs and close itself
   */
  protected abstract void save();
  protected abstract void saveAs();
  protected abstract void close();
  
  protected void addQuitMenuItem(JMenu menu) {

    final JMenuItem quit = new JMenuItem(Resources.getString(Resources.QUIT));
    quit.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.exit(0);
      }
    });
   
    quit.setMnemonic('Q');
    menuItems.put(MenuKey.QUIT, menu.add(quit));
  }
  
  protected void addTranslateMenuItem(JMenu menu) {

    final JMenuItem translateVASSAL = menu.add(Resources.getString(
        "Editor.ModuleEditor.translate_vassal")); //$NON-NLS-1$
    translateVASSAL.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new TranslateVassalWindow(EditorWindow.this).setVisible(true);
      }
    });

    menuItems.put(MenuKey.TRANSLATE_VASSAL, translateVASSAL);
  }

  protected void addUpdaterMenuItem(JMenu menu) {
    createUpdater = menu.add("Create " + getEditorType() + " updater");
    createUpdater.setEnabled(false);
    createUpdater.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new ModuleUpdaterDialog(EditorWindow.this).setVisible(true);
      }
    });

    addMenuItem(MenuKey.CREATE_MODULE_UPDATER, createUpdater);
  }
  
  protected void saver(final Runnable save) {
    final ValidationReport report = new ValidationReport();
    GameModule.getGameModule().validate(GameModule.getGameModule(), report);
    if (report.getWarnings().size() == 0) {
      save.run();
    }
    else {
      new ValidationReportDialog(report,
        new ValidationReportDialog.CallBack() {
          public void ok() {
            save.run();
          }
  
          public void cancel() {
          }
        }
      ).setVisible(true);
    }
  }
  
  /**
   * Called by the enclosed ConfigureTree or ExtensionTree when it's dirty
   * state is changed. The implementing class should override this if they
   * need to take action like changing menu availability.
   * 
   * @param changed true if the tree is in a changed (dirty) state
   */
  public void treeStateChanged(boolean changed) {
    
  }
}
