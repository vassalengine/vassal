/*
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman, Brent Easton
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuItem;

import VASSAL.build.GameModule;
import VASSAL.build.module.ModuleExtension;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.SavedGameUpdaterDialog;
import VASSAL.i18n.Resources;
import VASSAL.tools.imports.ImportAction;

public class ModuleEditorWindow extends EditorWindow {

  private static final long serialVersionUID = 1L;
  protected CreateModuleAction createModuleAction;
  protected EditModuleAction editModuleAction;
  protected ImportAction importAction;
  protected NewExtensionAction newExtensionAction;
  protected EditExtensionAction editExtensionAction;
  protected JMenuItem updateSavedGame;

  public static EditorWindow getInstance() {
    if (instance == null)
      instance = new ModuleEditorWindow();
    return instance;
  }

  /**
   * The GameModule is loading
   */
  public void moduleLoading(GameModule mod) {
    tree = new ConfigureTree(mod, helpWindow);
    scrollPane.setViewportView(tree);
    
    tree.buildEditMenu(getEditMenu());
    componentHelpItem.setAction(tree.getHelpAction());
    
    createModuleAction.setEnabled(false);
    editModuleAction.setEnabled(false);
    close.setEnabled(true);
    saveAction.setEnabled(true);
    saveAsAction.setEnabled(true);
    importAction.setEnabled(false);
    newExtensionAction.setEnabled(true);
    editExtensionAction.setEnabled(true);
    createUpdater.setEnabled(true);
    updateSavedGame.setEnabled(true);

    PlayerWindow.getInstance()
                .getMenuItem(PlayerWindow.MenuKey.OPEN_MODULE)
                .setEnabled(false);
    
    pack();
  }

  public void moduleLoading(GameModule mod, ModuleExtension ext) {

  }

  public String getEditorType() {
    return "Module";
  }

  protected void populateFileMenu(JMenu menu) {

    createModuleAction = new CreateModuleAction(this);
    addMenuItem(MenuKey.NEW, menu.add(createModuleAction));

    editModuleAction = new EditModuleAction(this);
    addMenuItem(MenuKey.OPEN, menu.add(editModuleAction));

    addCloseMenuItem(menu);
    addSaveMenuItem(menu);
    addSaveAsMenuItem(menu);

    menu.addSeparator();

    importAction = new ImportAction(this);
    addMenuItem(MenuKey.IMPORT, menu.add(importAction));

    menu.addSeparator();

    newExtensionAction = new NewExtensionAction(this);
    newExtensionAction.setEnabled(false);
    addMenuItem(MenuKey.NEW_EXTENSION, menu.add(newExtensionAction));

    editExtensionAction = new EditExtensionAction(this);
    editExtensionAction.setEnabled(false);
    addMenuItem(MenuKey.LOAD_EXTENSION, menu.add(editExtensionAction));

    menu.addSeparator();

    addQuitMenuItem(menu);
  }

  protected void populateToolsMenu(JMenu menu) {
    addUpdaterMenuItem(menu);

    updateSavedGame = menu.add(Resources
        .getString("Editor.ModuleEditor.update_saved")); //$NON-NLS-1$
    updateSavedGame.setEnabled(false);
    updateSavedGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        new SavedGameUpdaterDialog(ModuleEditorWindow.this).setVisible(true);
      }
    });

    addMenuItem(MenuKey.UPDATE_SAVED, updateSavedGame);

    menu.addSeparator();

    addTranslateMenuItem(menu);
  }

  protected void save() {
    ModuleEditorWindow.this.saver(new Runnable() {
      public void run() {
        GameModule.getGameModule().save();
      }
    });    
  }

  protected void saveAs() {
    ModuleEditorWindow.this.saver(new Runnable() {
      public void run() {
        GameModule.getGameModule().saveAs();
      }
    }); 
  }
  
  protected void close() {
    
  }
  
  

}