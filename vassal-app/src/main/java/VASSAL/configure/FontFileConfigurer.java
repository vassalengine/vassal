/*
 *
 * Copyright (c) 2006-2023 by Joel Uckelman, The VASSAL Development Team
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

import VASSAL.build.GameModule;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.FontFileFilter;
import VASSAL.tools.swing.SwingUtils;

public class FontFileConfigurer extends FileConfigurer {
  protected static DirectoryConfigurer fontDirPref;

  public FontFileConfigurer(String key, String name, ArchiveWriter archive) {
    super(key, name);
    this.archive = archive;
    setEditable(false);
  }

  @Override
  protected FileChooser initFileChooser() {
    if (fontDirPref == null) {
      fontDirPref = new DirectoryConfigurer("fonts", null); //NON-NLS
      GameModule.getGameModule().getPrefs().addOption(null, fontDirPref);
    }
    final FileChooser fc = FileChooser.createFileChooser(GameModule.getGameModule().getPlayerWindow(), fontDirPref);
    fc.setFileFilter(new FontFileFilter());
    return fc;
  }

  @Override
  protected void addToArchive(java.io.File f) {
    archive.addFont(f.getPath(), f.getName());
  }

  @Override
  public void setValue(Object o) {
    super.setValue(o);
    SwingUtils.repack(p);
  }
}