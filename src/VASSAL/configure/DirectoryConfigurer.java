/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.configure;

import java.io.File;

import VASSAL.build.GameModule;
import VASSAL.preferences.Prefs;
import VASSAL.tools.filechooser.FileChooser;

/**
 * A Configurer for picking file directories
 */
public class DirectoryConfigurer extends FileConfigurer {

  public DirectoryConfigurer(String key, String name) {
    super(key, name);
  }

  protected FileChooser initFileChooser() {
    FileChooser fc = FileChooser.createFileChooser(null, startingDirectory, FileChooser.DIRECTORIES_ONLY);
    if (startingDirectory == null && GameModule.getGameModule() != null) {
      fc.setCurrentDirectory((File) Prefs.getGlobalPrefs().getValue(Prefs.MODULES_DIR_KEY));
    }
    return fc;
  }

  public void setValue(Object o) {
    File f = (File) o;
    if (f != null && !f.isDirectory()) {
      f = null;
    }
    super.setValue(f);
  }

  protected void addToArchive(File f) {
  }

  public void chooseNewValue() {
    getControls();
    if (getValue() != null) {
      fc.setCurrentDirectory((File) getValue());
    }
    if (fc.showOpenDialog(getControls()) == FileChooser.APPROVE_OPTION) {
      setValue(fc.getSelectedFile());
    }
  }
}
