/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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

import java.awt.Taskbar;

import org.apache.commons.lang3.SystemUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.tools.icon.ModuleIcon;

/**
 * Shows a running module's own icon in the macOS Dock, so that each module
 * can be told apart from the Module Manager and from other modules.
 *
 * <p>Each module runs in its own Player JVM, which exits when the module is
 * closed, so the icon never needs to be put back.</p>
 */
public final class ModuleDockIcon {
  private static final Logger logger = LoggerFactory.getLogger(ModuleDockIcon.class);

  /** Size in pixels of the icon; the Dock scales it down. */
  static final int SIZE = 512;

  private ModuleDockIcon() {
  }

  /**
   * Sets the macOS Dock icon for the module just loaded. Does nothing on
   * other systems, or where the Dock icon can't be set. Never throws: a
   * failure leaves the default VASSAL icon in place.
   *
   * @param g the loaded module
   */
  public static void install(GameModule g) {
    if (!SystemUtils.IS_OS_MAC || g == null) {
      return;
    }

    try {
      if (!Taskbar.isTaskbarSupported()) {
        return;
      }
      final Taskbar taskbar = Taskbar.getTaskbar();
      if (!taskbar.isSupported(Taskbar.Feature.ICON_IMAGE)) {
        return;
      }
      taskbar.setIconImage(ModuleIcon.render(g, SIZE, ModuleIcon.Badge.VASSAL));
    }
    catch (RuntimeException e) {
      // a decoration only; never stop a module loading over it
      logger.warn("Unable to set the module Dock icon", e); //NON-NLS
    }
  }
}
