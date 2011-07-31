/*
 * $Id$
 *
 * Copyright (c) 2008 by Michael Kiefte
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

package VASSAL.tools.imports;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.JOptionPane;

import VASSAL.build.GameModule;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.ImportMetaData;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.launch.BasicModule;
import VASSAL.launch.EditModuleAction;
import VASSAL.launch.ModuleEditorWindow;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.filechooser.ExtensionFileFilter;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.FileFilter;
import VASSAL.tools.imports.adc2.ADC2Module;
import VASSAL.tools.imports.adc2.ADC2Utils;
import VASSAL.tools.imports.adc2.MapBoard;
import VASSAL.tools.imports.adc2.SymbolSet;

/**
 * Action for importing foreign modules into VASSAL.
 *
 * @author Michael Kiefte
 * @since 3.1.0
 */

/*
 * To add more capabilities, see the static fields DESCRIPTIONS, EXTENSIONS, and IMPORTERS.
 */

public final class ImportAction extends EditModuleAction {

  private static final long serialVersionUID = 1L;

  public ImportAction(Component comp) {
    super(comp);
    putValue(NAME, Resources.getString("Editor.import_module"));
  }

  /*
   * The following three arrays describe the import file types that we can handle.
   * They should be ordered in priority from most likely to least likely. File formats
   * with complex file headers should take greater priority.
   */

  private static final String[] EXTENSIONS = {
    ADC2Utils.MODULE_EXTENSION,
    ADC2Utils.MAP_EXTENSION,
    ADC2Utils.SET_EXTENSION,
  };

  private static final String[] DESCRIPTIONS = {
    ADC2Utils.MODULE_DESCRIPTION,
    ADC2Utils.MAP_DESCRIPTION,
    ADC2Utils.SET_DESCRIPTION,
  };

  /*
   * These classes must descend from Importer.
   */

  private static final Class<?>[] IMPORTERS = {
    ADC2Module.class,
    MapBoard.class,
    SymbolSet.class,
  };

  public static FileChooser getFileChooser(Component c) {
    final FileChooser chooser = FileChooser.createFileChooser(c,
      (DirectoryConfigurer)
        Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY));

    chooser.resetChoosableFileFilters();
    for (int i = IMPORTERS.length-1; i >= 0; --i) {
      chooser.addChoosableFileFilter(new ExtensionFileFilter(
        DESCRIPTIONS[i] + " (*" + EXTENSIONS[i].toLowerCase()
                        + ";*" + EXTENSIONS[i].toUpperCase() + ")",
        new String[] {EXTENSIONS[i]})
      );
    }

    return chooser;
  }

  public static Class<?> getImporterClass(File f) throws IOException {
    final int[] indices = new int[IMPORTERS.length];
    for (int i = 0; i < indices.length; ++i) {
      indices[i] = i;
    }

    final String s = '.' + Importer.getExtension(f.getName());
    for (int i = 0; i < EXTENSIONS.length; ++i) {
      if (EXTENSIONS[i].compareToIgnoreCase(s) == 0) {
        indices[0] = i;
        indices[i] = 0;
        break;
      }
    }

    for (int i = 0; i < indices.length; ++i) {
      try {
        if (((Importer) (IMPORTERS[indices[i]].newInstance())).isValidImportFile(f)) {
          return IMPORTERS[indices[i]];
        }
      }
      catch (InstantiationException e) {
        ErrorDialog.bug(e);
      }
      catch (IllegalAccessException e) {
        ErrorDialog.bug(e);
      }
    }

    return null;
  }

  public void performAction(ActionEvent e) throws IOException {
    actionCancelled = true;

    fc.resetChoosableFileFilters();
    for (int i = IMPORTERS.length-1; i >= 0; --i) {
      fc.addChoosableFileFilter(new ExtensionFileFilter(
          DESCRIPTIONS[i] + " (*" + EXTENSIONS[i].toLowerCase() + ";*" + EXTENSIONS[i].toUpperCase() + ")",
          new String[] {EXTENSIONS[i]})
      );
    }

    if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
      File f = fc.getSelectedFile();
      if (f != null && f.exists()) {
        loadModule(f);
        actionCancelled = false;
      }
    }
  }

  public void loadModule(File f) throws IOException {
    final Class<?> impClass = getImporterClass(f);
    if (impClass == null) {
      throw new FileFormatException("Unrecognized file format");
    }

    final GameModule module = new BasicModule(new ArchiveWriter((String) null));
    GameModule.init(module);

    final Importer imp;
    try {
      imp = (Importer) (impClass.newInstance());
      imp.importFile(this, f);
      imp.writeToArchive();
    }
    // these should never happen
    catch (IllegalAccessException e) {
      ErrorDialog.bug(e);
    }
    catch (InstantiationException e) {
      ErrorDialog.bug(e);
    }

    module.getFrame().setVisible(true);
    new ModuleEditorWindow(module).setVisible(true);
  }

  /**
   * Find case-insensitive, cross-platform match for a given Windows file. Will
   * ask the user if unable to locate the specified file.
   */
//  public File getCaseInsensitiveFile(File f) {
//    return getCaseInsensitiveFile(f, null, true, null);
//  }

//  public File getCaseInsensitiveFile(File f, boolean queryIfNotFound) {
//    return getCaseInsensitiveFile(f, null, queryIfNotFound, null);
//  }

  /**
   * Find case-insensitive, cross-platform match for a given Windows file. If unable
   * to find a match, will then search the directory of the second file for a match.
   * Will ask the user if still unable to locate the specified file.
   *
   * @param f    File to match with a Windows-specific file-name format.
   * @param base Another file whose directory to search for a match if unable to find otherwise.
   * @return     Local match
   */
//  public File getCaseInsensitiveFile(File f, File base) {
//    return getCaseInsensitiveFile(f, base, true, null);
//  }

//  File getCaseInsensitiveFile(File f, File base, boolean queryIfNotFound) {
//    return getCaseInsensitiveFile(f, base, queryIfNotFound, null);
//  }

  /**
   * Find case-insensitive, cross-platform match for a given Windows file. If unable
   * to find a match, will then search the directory of the second file for a match.
   * If still unable to locate the specified file will ask the user to locate the file
   * using the specified file filter.
   *
   * @param f      File to match with a Windows-specific file-name format.
   * @param base   Another file whose directory to search for a match.
   * @param filter <code>FileFilter</code> to use when asking the user to locate the match.
   * @return       Local match
   */
//  public File getCaseInsensitiveFile(File f, File base, FileFilter filter) {
//    return getCaseInsensitiveFile(f, base, true, filter);
//  }

  /**
   * Find case-insensitive, cross-platform match for a given Windows file.
   * If unable to locate the specified file will ask the user to locate the file
   * using the specified file filter.
   *
   * @param f      File to match with a Windows-specific file-name format.
   * @param filter <code>FileFilter</code> to use when asking the user to locate the match.
   * @return       Local match
   */
//  public File getCaseInsensitiveFile(File f, FileFilter filter) {
//    return getCaseInsensitiveFile(f, null, true, filter);
//  }

  /**
   * Find case-insensitive, cross-platform match for a given file specified for a Windows directory structure.
   *
   * @param f                File to search for formatted for Windows. E.g., "C:Dir\File.txt". The method will first
   *                         check to see if the file exists as formatted.  It will then search the path that this
   *                         file is in if that exists.
   * @param base             If unable to find the file, will look in the directory of <code>base</code> which
   *                         be localised to the current OS. Typically this is some default directory to look
   *                         for files. This must be a full file name -- not just the path.  The file name itself
   *                         doesn't matter. In the case of importing files, if a base file requires another file,
   *                         <code>base</code> is typically the base file under the assumption that the files on
   *                         which it depends are found in the same directory.  If <code>base</code> is <code>null</code>,
   *                         this will be skipped.
   * @param queryIfNotFound  If the file still cannot be found, after searching <code>f</code> and <code>base</code>,
   *                         should we ask the user to find it for us.
   * @param filter           The <code>FileFilter</code> that the file dialog can use to locate the file. If this is <code>null</code>,
   *                         then no filter is used.
   * @return                 If it exists, the file itself, otherwise null.
   */
  /*
   * This needs to be here as it uses the action's file chooser.
   */
  public File getCaseInsensitiveFile(File f, File base,
      boolean queryIfNotFound, FileFilter filter) {
    // Easy case
    if (f.exists())
      return f;

    final String name = Importer.getFileName(f.getName());

    // check files in same directory ignoring case
    final File parent = f.getParentFile();
    if (parent != null) {
      final File[] peers = parent.listFiles();
      if (peers != null) {
        for (File p : peers) {
          if (p.getName().equalsIgnoreCase(name))
            return p;
        }
      }
    }

    // if that doesn't work, check the files in the same directory as base
    if (base != null) {
      final File[] peers = base.getParentFile().listFiles();
      if (peers != null) {
        for (File p : peers) {
          if (p.getName().equalsIgnoreCase(name))
            return p;
        }
      }
    }

    // no luck so far.  Ask the user.
    if (queryIfNotFound) {
// FIXME: I18N!
      JOptionPane.showMessageDialog(comp, "Unable to locate file:\n"
          + f.getPath() + "\nPlease locate it in the following dialog.",
          "File Warning", JOptionPane.WARNING_MESSAGE);

      if (fc == null) {
        fc = getFileChooser(comp);
      }

      fc.resetChoosableFileFilters();
      if (filter != null) fc.setFileFilter(filter);

      fc.setSelectedFile(new File(f.getName()));

      if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
        final File p = fc.getSelectedFile();
        if (p.exists()) return p;
      }
    }

    // total failure
    return null;
  }

  public static AbstractMetaData buildMetaData(File module) {
    try {
      if (getImporterClass(module) == null) {
        // not a recognized file
        return null;
      }
    }
    catch (IOException e) {
      return null;
    }
    return new ImportMetaData();
  }
}
