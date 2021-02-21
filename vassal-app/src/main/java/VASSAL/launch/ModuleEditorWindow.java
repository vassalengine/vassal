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
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.Action;

import VASSAL.Info;
import org.apache.commons.lang3.StringUtils;

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

    setModuleName(mod.getDataArchive().getArchive().getFile().getName());

    scrollPane.setViewportView(tree);

    final MenuManager mm = MenuManager.getInstance();

    tree.populateEditMenu(this);

    mm.addAction("Editor.ModuleEditor.reference_manual", tree.getHelpAction());

    updateSavedGame = new AbstractAction(Resources.getString(
      "Editor.ModuleEditor.update_saved")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        new SavedGameUpdaterDialog(ModuleEditorWindow.this).setVisible(true);
      }
    };
    mm.addAction("Editor.ModuleEditor.update_saved", updateSavedGame);

    saveAction.setEnabled(true);
    saveAsAction.setEnabled(true);
    updateSavedGame.setEnabled(true);

    pack();

    tree.requestFocus(); //BR// Focus starts on the root node, not on the little icons on the toolbar
  }

  @Override
  public String getEditorType() {
    return Resources.getString("Editor.ModuleEditor.component_type");
  }

  @Override
  public void updateWindowTitle() {
    String title = Resources.getString("Editor.ModuleEditor.editor_name", Info.getVersion());

    if (!StringUtils.isEmpty(moduleName)) {
      title = moduleName + " - " + title;
    }

    setTitle(title);
  }

  @Override
  protected void save() {
    saver(() -> {
      final GameModule g = GameModule.getGameModule();
      g.save();
      final File f = g.getArchiveWriter().getArchive().getFile();
      setModuleName(f.getName());
      ModuleManagerUpdateHelper.sendModuleUpdate(f);
    });
  }

  @Override
  protected void saveAs() {
    saver(() -> {
      final GameModule g = GameModule.getGameModule();
      g.saveAs();
      final File f = g.getArchiveWriter().getArchive().getFile();
      setModuleName(f.getName());
      ModuleManagerUpdateHelper.sendModuleUpdate(f);
    });
  }
}
