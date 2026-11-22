/*
 *
 * Copyright (c) 2008 by Joel Uckelman
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
import VASSAL.tools.ErrorDialog;

import java.awt.Desktop;
import java.io.File;
import java.util.List;

import javax.swing.SwingUtilities;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ModuleManagerMacOSXStartUp extends MacOSXStartUp {
  private static final Logger logger =
    LoggerFactory.getLogger(ModuleManagerMacOSXStartUp.class);

  @Override
  public void initSystemProperties() {
    super.initSystemProperties();
    setupApplicationListeners();
  }

  /**
   * Handle files opened from the Finder (double-click, drag onto the
   * Dock icon, "Open With", or <code>open file.vmod</code>). macOS
   * delivers these as Apple Events rather than as command-line arguments,
   * both when VASSAL is started by the open and when it is already running.
   */
  protected void setupApplicationListeners() {
    final Desktop app = Desktop.getDesktop();
    app.setOpenFileHandler(e -> openFiles(e.getFiles()));
  }

  static void openFiles(List<File> files) {
    // Run on the EDT, queued behind the Module Manager window's creation,
    // so that a file opened at start up is handled once the window exists.
    SwingUtilities.invokeLater(() -> {
      for (final File f : files) {
        openFile(f.getAbsoluteFile());
      }
    });
  }

  private static void openFile(File file) {
    logger.info("Opening {} from the desktop", file); //NON-NLS

    final LaunchRequest lr;
    try {
      // classify the file exactly as the command line would
      lr = LaunchRequest.parseArgs(new String[] { "--load", file.getPath() }); //NON-NLS
    }
    catch (LaunchRequestException ex) {
      logger.warn("Cannot open {}: {}", file, ex.getMessage()); //NON-NLS
      ErrorDialog.show("Error.open_file_failed", file.getName(), ex.getMessage()); //NON-NLS
      return;
    }

    if (lr.module == null && lr.game != null) {
      // a save or log: it needs its module to be in the Module Manager
      final AbstractMetaData data = MetaDataFactory.buildMetaData(lr.game);
      final String moduleName = data instanceof SaveMetaData ?
        ((SaveMetaData) data).getModuleName() : null;
      if (moduleName == null || moduleName.isEmpty() ||
          ModuleManagerWindow.getInstance().getModuleByName(moduleName) == null) {
        logger.warn("No module found for {}", file); //NON-NLS
        ErrorDialog.show("Error.open_file_no_module", file.getName(), //NON-NLS
          moduleName == null || moduleName.isEmpty() ? "?" : moduleName);
        return;
      }
    }

    final LaunchRequestHandler handler = new LaunchRequestHandler(lr);
    handler.run();

    final String result = handler.getResult();
    if (result != null) {
      logger.warn("Cannot open {}: {}", file, result); //NON-NLS
      ErrorDialog.show("Error.open_file_failed", file.getName(), result); //NON-NLS
    }
  }
}
