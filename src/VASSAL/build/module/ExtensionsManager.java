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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import VASSAL.Info;
import VASSAL.build.GameModule;

/**
 * Convenience class for managing extensions relative to a module file
 * Create extension directory as lazily as possible
 * 
 * @author rodneykinney
 * 
 */
public class ExtensionsManager {
  // Preferences key for the list of extensions to load
  public static final String SPECIFY_DIR_IN_PREFS = "specifyExtensionDirInPrefs"; //$NON-NLS-1$
  public static final String EXTENSION_DIR = "extensionDIR"; //$NON-NLS-1$
  private File moduleFile;
  private File extensionsDir;
  private File inactiveDir;
  /**
   * Tests if the specified file should be accepted as an module extension file. Currently we disallow any files that
   * are hidden or "files" that are directories.
   */
  private FilenameFilter filter = new FilenameFilter() {
    public boolean accept(File dir, String name) {
      File fileCandidate = new File(dir, name);
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
    extensionsDir = ensureExists(new File(Info.getHomeDir(),dir));
  }

  public File getExtensionsDirectory(boolean mustExist) {
    if (extensionsDir == null) {
      File dir;
      if (GlobalOptions.getInstance() != null && "true".equals(GlobalOptions.getInstance().getAttributeValueString(SPECIFY_DIR_IN_PREFS))) { //$NON-NLS-1$
        dir = new File(GameModule.getGameModule().getPrefs().getOption(EXTENSION_DIR).getValueString());
      }
      else {
        String dirName = moduleFile.getPath();
        int index = dirName.lastIndexOf('.');
        if (index > 0) {
          dirName = dirName.substring(0, index);
        }
        dir = new File(dirName + "_ext");
        if (mustExist) {
          dir = ensureExists(dir);
        }
      }
      extensionsDir = dir;
    }
    if (mustExist && ! extensionsDir.exists()) {
      extensionsDir = ensureExists(extensionsDir);
    }
    return extensionsDir;
  }

  protected File ensureExists(File dir) {
    if (dir.exists() && !dir.isDirectory()) {
      throw new IllegalStateException(dir + " is not a directory");
    }
    if (!dir.exists()) {
      if (!dir.mkdirs()) {
        throw new IllegalStateException("Could not create " + dir);
      }
    }
    return dir;
  }

  public File getInactiveExtensionsDirectory(boolean mustExist) {
    if (inactiveDir == null) {
      inactiveDir = new File(getExtensionsDirectory(mustExist), "inactive");
      if (mustExist) {
        inactiveDir = ensureExists(inactiveDir);
      }
    }
    if (mustExist && ! inactiveDir.exists()) {
      inactiveDir = ensureExists(inactiveDir);
    }
    return inactiveDir;
  }

  public File setActive(File extension, boolean active) {
    File newExt;
    if (active) {
      newExt = new File(getExtensionsDirectory(true), extension.getName());
    }
    else {
      newExt = new File(getInactiveExtensionsDirectory(true), extension.getName());
    }
    extension.renameTo(newExt);
    return newExt;
  }

  public List<File> getActiveExtensions() {
    File dir = getExtensionsDirectory(false);
    return dir.exists() ? Arrays.asList(dir.listFiles(filter)) : new ArrayList<File>();
  }

  public List<File> getInactiveExtensions() {
    File dir = getInactiveExtensionsDirectory(false);
    return dir.exists() ? Arrays.asList(dir.listFiles(filter)) : new ArrayList<File>();
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
