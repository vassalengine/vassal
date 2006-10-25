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
package VASSAL.build.module;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

public class ExtensionsLoader implements CommandEncoder {
  // Preferences key for the list of extensions to load
  private static final String SPECIFY_DIR_IN_PREFS = "specifyExtensionDirInPrefs";
  private static final String EXTENSION_DIR = "extensionDIR";
  public static final String COMMAND_PREFIX = "EXT\t";

  private Set loadedExtensions = new HashSet();

  public void addTo(GameModule mod) {
    if ("true".equals(GlobalOptions.getInstance().getAttributeValueString(SPECIFY_DIR_IN_PREFS))) {
      DirectoryConfigurer config = new DirectoryConfigurer(EXTENSION_DIR, "Extensions Directory");
      config.setValue((Object) null);
      GameModule.getGameModule().getPrefs().addOption("Extensions", config);
      config.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          addExtensions();
        }
      });
    }
    mod.addCommandEncoder(this);
    addExtensions();
  }

  private void addExtensions() {
    String[] extensions = getExtensionNames();
    if (extensions != null) {
      for (int i = 0; i < extensions.length; ++i) {
        if (!loadedExtensions.contains(extensions[i])) {
          try {
            ModuleExtension ext = new ModuleExtension(new DataArchive(extensions[i]));
            ext.build();
            String msg = "Extension " + ext.getName() + " v" + ext.getVersion() + " loaded";
            loadedExtensions.add(extensions[i]);
            GameModule.getGameModule().warn(msg);
            System.err.println(msg);
          }
          catch (IOException e) {
            reportBuildError(e, extensions[i]);
          }
          catch (IllegalBuildException e) {
            reportBuildError(e, extensions[i]);
          }
        }
      }
    }
  }

  private void reportBuildError(Exception e, String name) {
    String msg = e.getMessage();
    if (msg == null || msg.length() == 0) {
      msg = e.getClass().getName();
      msg = msg.substring(msg.lastIndexOf('.'));
    }
    GameModule.getGameModule().warn("Unable to load extension " + name + ":  " + msg);
  }

  public Command decode(String command) {
    Command c = null;
    if (command.startsWith(COMMAND_PREFIX)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command.substring(COMMAND_PREFIX.length()), '\t');
      c = new ModuleExtension.RegCmd(st.nextToken(), st.nextToken());
    }
    return c;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof ModuleExtension.RegCmd) {
      ModuleExtension.RegCmd cmd = (ModuleExtension.RegCmd) c;
      SequenceEncoder se = new SequenceEncoder('\t');
      se.append(cmd.getName()).append(cmd.getVersion());
      s = COMMAND_PREFIX + se.getValue();
    }
    return s;
  }

  private String[] getExtensionNames() {
    String dirName = getExtensionDirectory();
    File dir = new File(dirName);
    String[] s = dir.list();
    if (s == null) {
      s = new String[0];
    }
    for (int i = 0; i < s.length; ++i) {
      s[i] = new File(dir, s[i]).getPath();
    }
    return s;
  }

  public static String getExtensionDirectory() {
    String dirName;
    if ("true".equals(GlobalOptions.getInstance().getAttributeValueString(SPECIFY_DIR_IN_PREFS))) {
      dirName = GameModule.getGameModule().getPrefs().getOption(EXTENSION_DIR).getValueString();
    }
    else {
      dirName = new File(GameModule.getGameModule().getDataArchive().getName()).getPath();
      int index = dirName.lastIndexOf('.');
      if (index > 0) {
        dirName = dirName.substring(0, index);
      }
      dirName = dirName + "_ext";
    }
    return dirName;
  }
}
