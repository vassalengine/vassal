/*
 * $Id$
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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

import java.awt.Component;

import javax.swing.JPanel;

import VASSAL.build.GameModule;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.filechooser.AudioFileFilter;
import VASSAL.tools.filechooser.FileChooser;

/**
 * Class for selecting an AudioClip while editing a module and adding it to
 * module
 *
 * @author rkinney
 *
 */
public class AudioClipConfigurer extends FileConfigurer {
  protected static DirectoryConfigurer resourceDirPref;
  protected FormattedExpressionConfigurer.ExpressionButton button;

  public AudioClipConfigurer(String key, String name, ArchiveWriter archive) {
    super(key, name);
    this.archive = archive;
  }

  protected FileChooser initFileChooser() {
    if (resourceDirPref == null) {
      resourceDirPref = new DirectoryConfigurer("audioDir", null);
      GameModule.getGameModule().getPrefs().addOption(null,resourceDirPref);
    }
    FileChooser fc = FileChooser.createFileChooser(GameModule.getGameModule().getFrame(),resourceDirPref);
    fc.setFileFilter(new AudioFileFilter());
    return fc;
  }

  protected void addToArchive(java.io.File f) {
    archive.addSound(f.getPath(), f.getName());
  }

  public Component getControls() {
    final Component c = super.getControls();
    if (button == null) {
      button = new FormattedExpressionConfigurer.ExpressionButton(this, tf.getPreferredSize().height);
      ((JPanel) c).add(button);
    }
    return c;
  }
}
