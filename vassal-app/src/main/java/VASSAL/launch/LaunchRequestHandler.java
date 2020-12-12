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

import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.SaveMetaData;

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
          final String moduleName = ((SaveMetaData) data).getModuleName();
          if (moduleName != null && moduleName.length() > 0) {
            // get the module file by module name
            lr.module = window.getModuleByName(moduleName);
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
}
