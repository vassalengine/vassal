/*
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman
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

import javax.swing.JFrame;
import javax.swing.JMenuBar;

import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;

public final class ModuleManagerMenuManager extends MenuManager {
  private final MenuBarProxy menuBar = new MenuBarProxy();

  @Override
  public JMenuBar getMenuBarFor(JFrame fc) {
    return (fc instanceof ModuleManagerWindow) ? menuBar.createPeer() : null;
  }

  @Override
  public MenuBarProxy getMenuBarProxyFor(JFrame fc) {
    return (fc instanceof ModuleManagerWindow) ? menuBar : null;
  }
}
