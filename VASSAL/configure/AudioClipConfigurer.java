/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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

import VASSAL.build.module.Documentation;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.AudioFileFilter;
import VASSAL.tools.FileChooser;

/**
 * Class for selecting an AudioClip while editing a module and adding it to
 * module
 * 
 * @author rkinney
 * 
 */
public class AudioClipConfigurer extends FileConfigurer {
  private static final FileChooser fc;

  static {
    fc = FileChooser.createFileChooser(null);
    fc.setCurrentDirectory(Documentation.getDocumentationBaseDir());
    fc.setFileFilter(new AudioFileFilter());
  }

  public AudioClipConfigurer(String key, String name, ArchiveWriter archive) {
    super(key, name);
    this.archive = archive;
  }

  protected FileChooser initFileChooser() {
    return fc;
  }

  protected void addToArchive(java.io.File f) {
    archive.addSound(f.getPath(), f.getName());
  }
}
