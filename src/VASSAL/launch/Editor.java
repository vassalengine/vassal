/*
 * $Id$
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Joel Uckelman 
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

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenuBar;

import org.apache.commons.lang.SystemUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.ImportMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.icon.IconFactory;
import VASSAL.tools.imports.ImportAction;
import VASSAL.tools.menu.MacOSXMenuManager;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;

public class Editor extends Launcher {
  public static void main(String[] args) {
    new Editor(args);
  }

  private static final Logger logger = LoggerFactory.getLogger(Editor.class);

  protected Editor(String[] args) {
    // the ctor is protected to enforce that it's called via main()
    super(args);
  }

  protected MenuManager createMenuManager() {
    return SystemUtils.IS_OS_MAC_OSX ?
      new MacOSXMenuManager() : new EditorMenuManager();
  }

  protected void launch() throws IOException {
    Object req = null;
    new IconFactory();  // Initialise the Icon Factory

    switch (lr.mode) {
    case EDIT:
      new EditModuleAction(lr.module).loadModule(lr.module);
      req = new AbstractLaunchAction.NotifyOpenModuleOk();
      break;
    case IMPORT:
      new ImportAction(null).loadModule(lr.importFile);
      req = new AbstractLaunchAction.NotifyImportModuleOk();
      break;
    case NEW:
      new CreateModuleAction(null).performAction(null);
      req = new AbstractLaunchAction.NotifyNewModuleOk();
      break;
    case EDIT_EXT:
      GameModule.init(new BasicModule(new DataArchive(lr.module.getPath())));
      GameModule.getGameModule().getFrame().setVisible(true);
      new EditExtensionAction(lr.extension).performAction(null);
      req = new AbstractLaunchAction.NotifyOpenModuleOk();
      break;
    case NEW_EXT:
      GameModule.init(new BasicModule(new DataArchive(lr.module.getPath())));
      final JFrame f = GameModule.getGameModule().getFrame();
      f.setVisible(true);
      new NewExtensionAction(f).performAction(null);
      req = new AbstractLaunchAction.NotifyOpenModuleOk();
    }

    if (cmdC != null) {
      try {
        cmdC.request(req);
      }
      catch (IOException e) {
        // This is not fatal, since we've successfully opened the module,
        // but possibly this means that the Module Manager has died.
        ErrorDialog.showDetails(
          e,
          ThrowableUtils.getStackTrace(e),
          "Error.socket_error"
        );
      }
    }
  }

  public static class NewModuleLaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public NewModuleLaunchAction(ModuleManagerWindow mm) {
      super(Resources.getString("Main.new_module"), mm,
        Editor.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.NEW)
      );
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask();
    }
  }

  public static class ImportLaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public ImportLaunchAction(ModuleManagerWindow mm, File module) {
      super(Resources.getString("Main.import_module"), mm,
        Editor.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.IMPORT, module)
      );
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask();
    }
  }

  public static class PromptImportLaunchAction extends ImportLaunchAction {
    private static final long serialVersionUID = 1L;

    public PromptImportLaunchAction(ModuleManagerWindow mm) {
      super(mm, null);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // prompt the user to pick a module
      if (promptForFile() == null) return;

      super.actionPerformed(e);
    }

    @Override
    protected File promptForFile() {
      // prompt the use to pick a module
      final FileChooser fc = ImportAction.getFileChooser(window);

      if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
        lr.importFile = fc.getSelectedFile();
        
        if (lr.importFile != null) {
          if (lr.importFile.exists()) {
            final AbstractMetaData metadata =
              MetaDataFactory.buildMetaData(lr.importFile);
            if (metadata == null || ! (metadata instanceof ImportMetaData)) {
              ErrorDialog.show(
                "Error.invalid_import_file", lr.importFile.getAbsolutePath());
              logger.error("Import of " + lr.importFile.getAbsolutePath() +
                " failed: unrecognized import type");
              lr.importFile = null;
            }
          }
          else {
            lr.importFile = null;
          }
        }
      } 
    
      return lr.importFile;
    }
  }

  public static class LaunchAction extends AbstractLaunchAction {
    private static final long serialVersionUID = 1L;

    public LaunchAction(ModuleManagerWindow mm, File module) {
      super(Resources.getString("Main.edit_module_specific"), mm,
        Editor.class.getName(),
        new LaunchRequest(LaunchRequest.Mode.EDIT, module)
      );
      setEnabled(!editing.contains(module) && !using.containsKey(module));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // register that this module is being edited
      if (editing.contains(lr.module) || using.containsKey(lr.module)) return;
      editing.add(lr.module);

      super.actionPerformed(e);
    }

    @Override
    protected LaunchTask getLaunchTask() {
      return new LaunchTask() {
        @Override
        protected void done() {
          super.done();

          // register that this module is no longer being edited
          editing.remove(lr.module);
          setEnabled(true);
        }
      };
    }
  }

  public static class ListLaunchAction extends LaunchAction {
    private static final long serialVersionUID = 1L;

    public ListLaunchAction(ModuleManagerWindow mm, File module) {
      super(mm, module);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      super.actionPerformed(e);
      setEnabled(false);
    }
  }

  public static class PromptLaunchAction extends LaunchAction {
    private static final long serialVersionUID = 1L;

    public PromptLaunchAction(ModuleManagerWindow mm) {
      super(mm, null);
      putValue(Action.NAME, Resources.getString("Main.edit_module"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      // prompt the user to pick a module
      if (promptForFile() == null) return;

      final AbstractMetaData data = MetaDataFactory.buildMetaData(lr.module);
      if (data != null && Info.isModuleTooNew(data.getVassalVersion())) {
        ErrorDialog.show(
          "Error.module_too_new",
          lr.module.getPath(),
          data.getVassalVersion(),
          Info.getVersion()
        );
        return;
      }
      super.actionPerformed(e);
    }
  }

  private static class EditorMenuManager extends MenuManager {
    private final MenuBarProxy editorBar = new MenuBarProxy();
    private final MenuBarProxy playerBar = new MenuBarProxy();

    @Override
    public JMenuBar getMenuBarFor(JFrame fc) {
      if (fc instanceof PlayerWindow) return playerBar.createPeer();
      else if (fc instanceof EditorWindow) return editorBar.createPeer();
      else return null;
    }

    @Override
    public MenuBarProxy getMenuBarProxyFor(JFrame fc) {
      if (fc instanceof PlayerWindow) return playerBar;
      else if (fc instanceof EditorWindow) return editorBar;
      else return null;
    }
  }
}
