/*
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

import VASSAL.Info;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

import java.io.IOException;
import java.util.List;

import javax.swing.JOptionPane;

public final class LaunchRequestHandler implements Runnable {
  private final LaunchRequest lr;
  private String result;

  public LaunchRequestHandler(LaunchRequest lr) {
    this.lr = lr;
  }

  @Override
  public void run() {
    result = handle();
  }

  public String getResult() {
    return result;
  }

  private String handle() {
    final ModuleManagerWindow window = ModuleManagerWindow.getInstance();

    switch (lr.mode) {
    case MANAGE:
      window.toFront();
      break;
    case LOAD:
      if (Player.LaunchAction.isEditing(lr.module)) {
        return "module open for editing";   // FIXME
      }

      if (lr.module == null && lr.game != null) {
        // attempt to find the module for the saved game or log
        final AbstractMetaData data = MetaDataFactory.buildMetaData(lr.game);
        if (data instanceof SaveMetaData) {
          // we found save metadata
          final SaveMetaData saveData = (SaveMetaData) data;
          final String moduleName = saveData.getModuleName();
          if (moduleName != null && moduleName.length() > 0) {
            // get the module file by module name and version
            final String moduleVersion =
              ModuleVersionMatcher.fullVersion(saveData.getModuleData());
            lr.module = window.getModuleForSave(moduleName, moduleVersion);
            if (lr.module == null) {
              final List<String> installed =
                window.getModuleVersions(moduleName);
              final ModuleManagerWindow.ModuleInfo tooNew =
                window.findModuleForSave(moduleName, moduleVersion, true);
              if (tooNew != null) {
                // the version which fits needs a newer VASSAL
                ErrorDialog.show(
                  "Error.module_too_new", //NON-NLS
                  tooNew.getFile().getPath(),
                  tooNew.getVassalVersion(),
                  Info.getVersion()
                );
              }
              else if (installed.isEmpty()) {
                ErrorDialog.show(
                  "Error.save_module_not_installed", //NON-NLS
                  lr.game.getName(), moduleName
                );
              }
              else {
                ErrorDialog.show(
                  "Error.save_module_version_not_installed", //NON-NLS
                  lr.game.getName(), moduleName, moduleVersion,
                  String.join(", ", installed)
                );
              }
              return "cannot find module"; //NON-NLS
            }

            if (!extensionsOk(window)) {
              return "cancelled"; //NON-NLS
            }
          }
          else {
            // this is a pre 3.1 save file, can't tell the module name
// FIXME: show some error here
            return "cannot find module";
          }
        }
      }

      if (lr.module == null) {
        return "cannot find module";
// FIXME: show some error here
      }
      else if (lr.game == null) {
        new Player.LaunchAction(window, lr.module).actionPerformed(null);
      }
      else {
        new Player.LaunchAction(window, lr.module, lr.game).actionPerformed(null);
      }
      break;
    case EDIT:
      if (Editor.LaunchAction.isInUse(lr.module)) {
        return "module open for play";      // FIXME
      }

      if (Editor.LaunchAction.isEditing(lr.module)) {
        return "module open for editing";   // FIXME
      }

      new Editor.LaunchAction(window, lr.module).actionPerformed(null);
      break;
    case IMPORT:
      new Editor.ImportLaunchAction(window, lr.importFile).actionPerformed(null);
      break;
    case NEW:
      new Editor.NewModuleLaunchAction(window).actionPerformed(null);
      break;
    case EDIT_EXT:
      return "not yet implemented";   // FIXME
    case NEW_EXT:
      return "not yet implemented";   // FIXME
    case UPDATE_MOD:
      window.updateRequest(lr.module);
      break;
    case UPDATE_EXT:
      window.updateRequest(lr.extension);
      break;
    case UPDATE_GAME:
      window.updateRequest(lr.game);
      break;
    default:
      return "unrecognized mode";     // FIXME
    }

    return null;
  }

  /**
   * Warns if the save needs extensions which the chosen module will not
   * load, or will load only in an older version.
   *
   * @return true to go ahead with the launch
   */
  private boolean extensionsOk(ModuleManagerWindow window) {
    final List<SaveExtensions.Extension> needed;
    try {
      needed = SaveExtensions.read(lr.game);
    }
    catch (IOException e) {
      // loading the save reports a file it cannot read
      return true;
    }

    final List<SaveExtensions.Shortfall> shortfalls =
      window.checkSaveExtensions(lr.module, needed);
    if (shortfalls.isEmpty()) {
      return true;
    }

    final StringBuilder message = new StringBuilder(
      Resources.getString("ModuleManager.save_extensions_header", lr.game.getName())
    ).append("\n\n");
    for (final SaveExtensions.Shortfall s : shortfalls) {
      final String key;
      switch (s.problem) {
      case INACTIVE:
        key = "ModuleManager.save_extension_inactive"; //NON-NLS
        break;
      case OLDER:
        key = "ModuleManager.save_extension_older"; //NON-NLS
        break;
      default:
        key = "ModuleManager.save_extension_missing"; //NON-NLS
        break;
      }
      message.append("\u2022 ")
        .append(Resources.getString(key, s.needed.toString(), s.installedVersion))
        .append('\n');
    }
    message.append('\n')
      .append(Resources.getString("ModuleManager.save_extensions_trailer"));

    return JOptionPane.showConfirmDialog(
      window,
      message.toString(),
      Resources.getString("ModuleManager.save_extensions_title"),
      JOptionPane.YES_NO_OPTION,
      JOptionPane.WARNING_MESSAGE) == JOptionPane.YES_OPTION;
  }
}
