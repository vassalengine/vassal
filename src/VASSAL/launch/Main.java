/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.launch;

import VASSAL.tools.ErrorDialog;

/**
 * @deprecated Use {@link Editor}, {@link Player}, and {@link ModuleManager}
 *  as entry points for VASSAL instead.
 */
@Deprecated
public class Main {
  public static void main(String[] args) {
    System.setProperty("swing.aatext", "true"); //$NON-NLS-1$ //$NON-NLS-2$
    System.setProperty("swing.boldMetal", "false"); //$NON-NLS-1$ //$NON-NLS-2$
    System.setProperty("awt.useSystemAAFontSettings", "on"); //$NON-NLS-1$ //$NON-NLS-2$

    ErrorDialog.show(
      null, null,
      "Obsolete Entry Point",
      "The Entry Point VASSAL.launch.Main is Obsolete",
      "You have attempted to start VASSAL from the VASSAL.launch.Main entry point. This entry point is no longer current. The current entry points are VASSAL.launch.ModuleManager, VASSAL.launch.Player, and VASSAL.launch.Editor.\n\nIf this message makes no sense to you, or you were trying to load a module, please ask for help at the VASSAL Forum."
    );
  }
}
