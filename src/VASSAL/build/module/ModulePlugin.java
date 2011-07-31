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

import VASSAL.command.Command;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;

/**
 * A ModulePlugin is basically identical to a ModuleExtension except
 * that is loaded earlier and must throw Plugin specific error messages
 */
public class ModulePlugin extends ModuleExtension {

  public ModulePlugin(DataArchive archive) {
    super(archive);
  }

  public Command getRestoreCommand() {
    return new RegCmd(getName(), getVersion());
  }

  public static class RegCmd extends ModuleExtension.RegCmd {

    public RegCmd(String name, String version) {
      super(name, version);
    }

    protected String getVersionErrorMsg(String v) {
      return Resources.getString("ModulePlugin.wrong_plugin_version", //$NON-NLS-1$
          getVersion(), getName(), v);
    }

    protected String getNotLoadedMsg() {
      return Resources.getString("ModulePlugin.load_plugin", getName(), PluginsLoader.getPluginDirectory()); //$NON-NLS-1$
    }
  }
}