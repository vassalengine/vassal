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
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ImageFileFilter;

/**
 * A Configurer for {@link java.awt.Image} values. Allows the user to select an
 * image file and writes it to a {@link ArchiveWriter}.
 */
public class ImageConfigurer extends FileConfigurer {
  protected static DirectoryConfigurer resourceDirPref;

  public ImageConfigurer(String key, String name, ArchiveWriter archive) {
    super(key, name);
    this.archive = archive;
    editable = false;
  }

  protected FileChooser initFileChooser() {
    final GameModule gm = GameModule.getGameModule();

    if (resourceDirPref == null) {
      resourceDirPref = new DirectoryConfigurer("imageDir", null);
      gm.getPrefs().addOption(null,resourceDirPref);
    }

    final FileChooser fc =
      FileChooser.createFileChooser(gm.getFrame(), resourceDirPref);
    fc.setFileFilter(new ImageFileFilter());
    return fc;
  }

  @Override
  protected void addToArchive(File f) {
    archive.addImage(f.getPath(), f.getName());
  }
}
