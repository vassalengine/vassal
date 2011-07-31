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

import javax.swing.AbstractAction;
import javax.swing.Action;

import VASSAL.build.GameModule;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.SavedGameUpdaterDialog;
import VASSAL.i18n.Resources;
import VASSAL.tools.menu.MenuManager;

public class ModuleEditorWindow extends EditorWindow {
  private static final long serialVersionUID = 1L;

  protected Action updateSavedGame;

  public ModuleEditorWindow(GameModule mod) {
    super();
    tree = new ConfigureTree(mod, helpWindow, this);
    treeStateChanged(false);
    scrollPane.setViewportView(tree);

    final MenuManager mm = MenuManager.getInstance();

    tree.populateEditMenu(this);

    mm.addAction("Editor.ModuleEditor.reference_manual", tree.getHelpAction());

    updateSavedGame = new AbstractAction(Resources.getString(
                          "Editor.ModuleEditor.update_saved")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        new SavedGameUpdaterDialog(ModuleEditorWindow.this).setVisible(true);
      }
    };
    mm.addAction("Editor.ModuleEditor.update_saved", updateSavedGame);

    saveAction.setEnabled(true);
    saveAsAction.setEnabled(true);
    createUpdater.setEnabled(true);
    updateSavedGame.setEnabled(true);

    pack();
  }

  public String getEditorType() {
    return "Module";
  }

/*
  protected void populateFileMenu(JMenu menu) {
    addSaveMenuItem(menu);
    addSaveAsMenuItem(menu);
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
*/

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
}
