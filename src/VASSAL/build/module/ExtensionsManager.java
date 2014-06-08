/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.build.module;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.ExtensionMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.WriteErrorDialog;

/**
 * Convenience class for managing extensions relative to a module file.
 * Create extension directory as lazily as possible.
 *
 * @author rodneykinney
 *
 */

public class ExtensionsManager {
  private File moduleFile;
  private File extensionsDir;
  private File inactiveDir;

  /**
   * Tests if the specified file should be accepted as an module extension
   * file. Currently we disallow any files that are hidden or "files" that
   * are directories.
   */
  private final FilenameFilter filter = new FilenameFilter() {
    public boolean accept(File dir, String name) {
      final File fileCandidate = new File(dir, name);
      return !fileCandidate.isHidden() && !fileCandidate.isDirectory();
    }
  };

  public ExtensionsManager(File moduleFile) {
    this.moduleFile = moduleFile;
  }

  public ExtensionsManager(GameModule module) {
    this.moduleFile = new File(GameModule.getGameModule().getDataArchive().getName());
  }

  /**
   * Manage global extensions
   */
  public ExtensionsManager(String dir) {
    extensionsDir = ensureExists(new File(Info.getHomeDir(), dir));
  }

  public File getExtensionsDirectory(boolean mustExist) {
    if (extensionsDir == null && moduleFile != null) {
      File dir;
      String dirName = moduleFile.getPath();
      int index = dirName.lastIndexOf('.');
      if (index > 0) {
        dirName = dirName.substring(0, index);
      }
      dir = new File(dirName + "_ext");
      if (mustExist) {
        dir = ensureExists(dir);
      }
      extensionsDir = dir;
      if (extensionsDir == null) {
        return null;
      }
    }
    if (mustExist && !extensionsDir.exists()) {
      extensionsDir = ensureExists(extensionsDir);
    }
    return extensionsDir;
  }

  public void setExtensionsDirectory(File dir) {
    extensionsDir = dir == null ? null : ensureExists(dir);
  }


  /**
   * Ensure a directory exists.
   * @param dir Directory
   * @return Directory as <code>File</code> object; otherwise <code>null</code> if an error occurs.
   */
  protected File ensureExists(File dir) {
    if (dir.exists() && !dir.isDirectory()) {
      WriteErrorDialog.error(new IOException(dir + "is not a directory"), dir);
      return null;
    }
    else if (!dir.exists() && !dir.mkdirs()) {
      WriteErrorDialog.error(new IOException("Could not create " + dir), dir);
      return null;
    }
    return dir;
  }

  public File getInactiveExtensionsDirectory(boolean mustExist) {
    if (inactiveDir == null) {
      File extDir = getExtensionsDirectory(mustExist);
      if (extDir == null) {
        return null;
      }
      inactiveDir = new File(extDir, "inactive");
      if (mustExist) {
        inactiveDir = ensureExists(inactiveDir);
        if ( inactiveDir == null ) {
          return null;
        }
      }
    }
    if (mustExist && !inactiveDir.exists()) {
      inactiveDir = ensureExists(inactiveDir);
    }
    return inactiveDir;
  }

  public File setActive(File extension, boolean active) {
    File newExt;
    if (active) {
      final File extensionsDirectory = getExtensionsDirectory(true);
      if (extensionsDirectory == null) {
        return extension;
      }
      newExt = new File(extensionsDirectory, extension.getName());
    }
    else {
      final File inactiveExtensionsDirectory = getInactiveExtensionsDirectory(true);
      if (inactiveExtensionsDirectory == null) {
        return extension;
      }
      newExt = new File(inactiveExtensionsDirectory, extension.getName());
    }
    extension.renameTo(newExt);
    return newExt;
  }

  private List<File> getExtensions(File dir) {
    final List<File> extensions = new ArrayList<File>(0);
    if (dir != null && dir.exists()) {
      File[] files = dir.listFiles(filter);
      if (files == null) {
        ReadErrorDialog.error(new IOException(), dir);
      }
      else {
        for (File file : files) {
          final AbstractMetaData metadata = MetaDataFactory.buildMetaData(file);
          if (metadata != null && metadata instanceof ExtensionMetaData) {
            extensions.add(file);
          }
        }

      }
    }
    return extensions;
  }

  public List<File> getActiveExtensions() {
    return getExtensions(getExtensionsDirectory(false));
  }

  public List<File> getInactiveExtensions() {
    return getExtensions(getInactiveExtensionsDirectory(false));
  }

  public boolean isExtensionActive(File extension) {
    for (File f : getActiveExtensions()) {
      if (f.getName().equals(extension.getName())) {
        return true;
      }
    }
    return false;
  }
}
