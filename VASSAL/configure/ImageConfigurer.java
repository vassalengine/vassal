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
import VASSAL.tools.ArchiveWriter;

/**
 * A Configurer for {@link java.awt.Image} values. Allows the user to select an
 * image file and writes it to a {@link ArchiveWriter}.
 */
public class ImageConfigurer extends FileConfigurer {
  private static final JFileChooser fc = new Chooser();

  public ImageConfigurer(String key, String name, ArchiveWriter archive) {
    super(key, name);
    this.archive = archive;
    editable = false;
  }
  
  protected JFileChooser initFileChooser() {
    return fc;
  }

  protected void addToArchive(File f) {
    archive.addImage(f.getPath(), f.getName());
  }

  private static class Chooser extends JFileChooser {
    private Chooser() {
      super(Documentation.getDocumentationBaseDir());
      setFileFilter(new javax.swing.filechooser.FileFilter() {
        public boolean accept(java.io.File f) {
          String name = f.getName();
          return name.endsWith(".gif") || name.endsWith(".GIF") || name.endsWith(".jpg") || name.endsWith(".JPG") || name.endsWith(".png")
              || name.endsWith(".PNG") || f.isDirectory();
        }

        public String getDescription() {
          return "Image files";
        }
      });
    }
  }
}
