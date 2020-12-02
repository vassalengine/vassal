/*
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

  private final ExtensionsManager extMgr = new ExtensionsManager("plugins"); //NON-NLS

  @Override
  public void addTo(GameModule mod) {
    mod.addCommandEncoder(this);
    for (final File ext : extMgr.getActiveExtensions()) {
      addExtension(ext);
    }
  }

  @Override
  protected ModuleExtension createExtension(String extname)
                                            throws ZipException, IOException {
    return new ModulePlugin(new DataArchive(extname));
  }

  @Override
  public Command decode(String command) {
    if (command.startsWith(COMMAND_PREFIX)) {
      final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command.substring(COMMAND_PREFIX.length()), '\t');
      return new ModulePlugin.RegCmd(st.nextToken(), st.nextToken());
    }

    return super.decode(command);
  }

  @Override
  public String encode(Command c) {
    if (c instanceof ModulePlugin.RegCmd) {
      final ModulePlugin.RegCmd cmd = (ModulePlugin.RegCmd) c;
      final SequenceEncoder se = new SequenceEncoder('\t');
      se
        .append(cmd.getName())
        .append(cmd.getVersion());
      return COMMAND_PREFIX + se.getValue();
    }

    return super.encode(c);
  }

  public static String getPluginDirectory() {
    return new ExtensionsManager("plugins").getExtensionsDirectory(false).getPath();  //$NON-NLS-1$
  }

  @Override
  protected String getLoadedMessage(String name, String version) {
    return Resources.getString("PluginsLoader.plugin_loaded", name, version); //$NON-NLS-1$
  }

  @Override
  protected String getErrorMessage(String name, String msg) {
    return Resources.getString("PluginsLoader.unable_to_load", name, msg);  //$NON-NLS-1$
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
