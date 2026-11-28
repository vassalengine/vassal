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
import VASSAL.build.module.metadata.ExtensionMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.version.VersionUtils;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.swing.JComboBox;
import javax.swing.JOptionPane;
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
    // Run on the EDT. A file opened at start up can arrive before
    // ModuleManager.launch() has run; ModuleManagerWindow.getInstance()
    // then creates the window, and launch() shows that same window.
    SwingUtilities.invokeLater(() -> {
      for (final File f : files) {
        openFile(f.getAbsoluteFile());
      }
    });
  }

  private static void openFile(File file) {
    logger.info("Opening {} from the desktop", file); //NON-NLS

    // the command line has no way to install an extension, so handle
    // extensions here rather than through LaunchRequest
    final AbstractMetaData fileData = MetaDataFactory.buildMetaData(file);
    if (fileData instanceof ExtensionMetaData) {
      installExtension(file, (ExtensionMetaData) fileData);
      return;
    }

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

  /**
   * An extension opened from the Finder: offer to add it to its module,
   * as Add Extension... in the Module Manager does, and to play that
   * module with it. The file is copied, so the original stays put.
   */
  private static void installExtension(File file, ExtensionMetaData ext) {
    final String moduleName = ext.getModuleName();
    final ModuleManagerWindow window = ModuleManagerWindow.getInstance();

    // a universal extension works with any module
    final List<File> modules = window.getModulesByName(
      ext.isUniversal() ? null : moduleName);
    if (modules.isEmpty()) {
      logger.warn("No module found for {}", file); //NON-NLS
      ErrorDialog.show("Error.open_file_no_module", file.getName(), //NON-NLS
        moduleName == null || moduleName.isEmpty() ? "?" : moduleName);
      return;
    }

    window.toFront();

    // Preselect the newest version the extension was built for or later;
    // extensions only warn when the module is older than that.
    // A universal extension can go anywhere, but usually names its module.
    final List<File> named = ext.isUniversal() ?
      window.getModulesByName(moduleName == null ? "" : moduleName) : modules;
    final String builtFor = ext.getModuleVersion();
    File preferred = null;
    File newest = null;
    for (final File m : named) {
      final String v = window.getModuleVersion(m);
      if (newest == null ||
          VersionUtils.compareVersions(v, window.getModuleVersion(newest)) > 0) {
        newest = m;
      }
      if ((builtFor == null || builtFor.isEmpty() ||
           VersionUtils.compareVersions(v, builtFor) >= 0) &&
          (preferred == null ||
           VersionUtils.compareVersions(v, window.getModuleVersion(preferred)) > 0)) {
        preferred = m;
      }
    }
    if (preferred == null) {
      preferred = newest == null ? modules.get(0) : newest;
    }

    final String[] labels = new String[modules.size()];
    for (int i = 0; i < labels.length; i++) {
      final File m = modules.get(i);
      labels[i] = Resources.getString("ModuleManager.open_extension_module", //NON-NLS
        m.getName(), window.getModuleVersion(m));
    }
    final JComboBox<String> choice = new JComboBox<>(labels);
    choice.setSelectedIndex(modules.indexOf(preferred));

    final Object message = modules.size() == 1 ?
      Resources.getString("ModuleManager.open_extension_add", //NON-NLS
        file.getName(), labels[0]) :
      new Object[] {
        Resources.getString("ModuleManager.open_extension_choose", //NON-NLS
          file.getName()),
        choice
      };

    final String addAndPlay =
      Resources.getString("ModuleManager.open_extension_add_and_play"); //NON-NLS
    final String add = Resources.getString("ModuleManager.open_extension_add_only"); //NON-NLS
    final String cancel = Resources.getString("General.cancel"); //NON-NLS
    final Object[] options = { addAndPlay, add, cancel };

    final int answer = JOptionPane.showOptionDialog(
      window, message,
      Resources.getString("ModuleManager.open_extension_title"), //NON-NLS
      JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE,
      null, options, add);
    if (answer != 0 && answer != 1) {
      return;
    }

    final File module = modules.get(Math.max(0, choice.getSelectedIndex()));
    logger.info("Extension {}: {} chosen, answer {}", file, module, options[answer]); //NON-NLS

    // a universal extension may be added to any module, but say so when
    // it wasn't built for the one chosen
    if (ext.isUniversal() && moduleName != null && !moduleName.isEmpty() &&
        !named.contains(module) &&
        JOptionPane.showConfirmDialog(
          window,
          Resources.getString("ModuleManager.open_extension_other_module", //NON-NLS
            file.getName(), moduleName, module.getName()),
          Resources.getString("ModuleManager.open_extension_title"), //NON-NLS
          JOptionPane.OK_CANCEL_OPTION,
          JOptionPane.WARNING_MESSAGE) != JOptionPane.OK_OPTION) {
      return;
    }

    final File dir = window.getExtensionsDirectory(module);
    final File target = dir == null ? null : new File(dir, file.getName());
    if (target != null && target.exists() &&
        !target.getAbsoluteFile().equals(file.getAbsoluteFile()) &&
        JOptionPane.showConfirmDialog(
          window,
          Resources.getString("ModuleManager.open_extension_replace", //NON-NLS
            file.getName(), module.getName()),
          Resources.getString("ModuleManager.open_extension_title"), //NON-NLS
          JOptionPane.OK_CANCEL_OPTION,
          JOptionPane.QUESTION_MESSAGE) != JOptionPane.OK_OPTION) {
      return;
    }

    try {
      window.installExtension(module, file);
    }
    catch (IOException ex) {
      logger.warn("Cannot add {} to {}", file, module, ex); //NON-NLS
      ErrorDialog.show("Error.open_file_failed", file.getName(), ex.getMessage()); //NON-NLS
      return;
    }

    if (answer == 0) {
      if (AbstractLaunchAction.isInUse(module) ||
          AbstractLaunchAction.isEditing(module)) {
        // a running module only loads extensions when it starts
        JOptionPane.showMessageDialog(
          window,
          Resources.getString("ModuleManager.open_extension_running", //NON-NLS
            file.getName(), module.getName()),
          Resources.getString("ModuleManager.open_extension_title"), //NON-NLS
          JOptionPane.INFORMATION_MESSAGE);
      }
      else {
        new Player.LaunchAction(window, module).actionPerformed(null);
      }
    }
  }
}
