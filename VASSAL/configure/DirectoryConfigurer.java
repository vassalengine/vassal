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

import javax.swing.JFileChooser;

import VASSAL.build.module.Documentation;

/**
 * A Configurer for picking file directories
 */
public class DirectoryConfigurer extends FileConfigurer {
  private static final JFileChooser fc = new Pan();

  public DirectoryConfigurer(String key, String name) {
    super(key, name);
    setValue(new File(System.getProperty("user.home")));
  }

  public void setValue(Object o) {
    File f = (File) o;
    if (f != null
      && !f.isDirectory()) {
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
    if (fc.showOpenDialog(null) != JFileChooser.CANCEL_OPTION) {
      setValue(fc.getSelectedFile());
    }
  }

  private static class Pan extends JFileChooser {
    private Pan() {
      setCurrentDirectory(Documentation.getDocumentationBaseDir());
      setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
      setFileFilter(new javax.swing.filechooser.FileFilter() {
        public boolean accept(File f) {
          return f.isDirectory();
        }

        public String getDescription() {
          return "Directory";
        }
      });
    }
  }
}


