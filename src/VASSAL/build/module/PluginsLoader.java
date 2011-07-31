/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney, Brent Easton
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
import java.io.IOException;
import java.util.zip.ZipException;

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

/**
 * Load Plugins.
 * @author Brent Easton
 *
 */
public class PluginsLoader extends ExtensionsLoader {

  public static final String COMMAND_PREFIX = "PLUGIN\t"; //$NON-NLS-1$

  private ExtensionsManager extMgr = new ExtensionsManager("plugins");

  public void addTo(GameModule mod) {
    mod.addCommandEncoder(this);
    for (File ext : extMgr.getActiveExtensions()) {
      addExtension(ext);
    }
  }

  @Override
  protected ModuleExtension createExtension(String extname)
                                            throws ZipException, IOException {
    return new ModulePlugin(new DataArchive(extname));
  }

  public Command decode(String command) {
    Command c = null;
    if (command.startsWith(COMMAND_PREFIX)) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command.substring(COMMAND_PREFIX.length()), '\t');
      c = new ModulePlugin.RegCmd(st.nextToken(), st.nextToken());
    }
    else {
      c = super.decode(command);
    }
    return c;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof ModulePlugin.RegCmd) {
      ModulePlugin.RegCmd cmd = (ModulePlugin.RegCmd) c;
      SequenceEncoder se = new SequenceEncoder('\t');
      se.append(cmd.getName()).append(cmd.getVersion());
      s = COMMAND_PREFIX + se.getValue();
    }
    else {
      s = super.encode(c);
    }
    return s;
  }

  public static String getPluginDirectory() {
    return new ExtensionsManager("plugins").getExtensionsDirectory(false).getPath();  //$NON-NLS-1$
  }

  protected String getLoadedMessage(String name, String version) {
    return Resources.getString("PluginsLoader.plugin_loaded", name, version); //$NON-NLS-1$
  }

  protected String getErrorMessage(String name, String msg) {
    return Resources.getString("PluginsLoader.unable_to_load", name , msg);  //$NON-NLS-1$
  }

  /**
   * Any components that are added to the module by a Plugin MUST
   * implement PluginElement to prevent them being written to the
   * buildFile when saving the module. Implemented by Plugin and
   * ModuleExtension.
   */
  public interface PluginElement {
  }
}
