/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
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

package VASSAL.tools.menu;

import java.awt.Desktop;
import java.awt.desktop.AboutHandler;
import java.awt.desktop.AboutEvent;
import java.awt.desktop.PreferencesHandler;
import java.awt.desktop.PreferencesEvent;
import java.awt.desktop.QuitHandler;
import java.awt.desktop.QuitEvent;
import java.awt.desktop.QuitResponse;

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenuBar;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class MacOSXMenuManager extends MenuManager {
  private final MenuBarProxy menuBar = new MenuBarProxy();

  @Override
  public JMenuBar getMenuBarFor(JFrame fc) {
    return menuBar.createPeer();
  }

  @Override
  public MenuBarProxy getMenuBarProxyFor(JFrame fc) {
    return menuBar;
  }

  @Override
  public MenuItemProxy addKey(String key) {
    // don't reserve slots for Quit, Preferences, or About on Macs
    if ("General.quit".equals(key) ||
        "Prefs.edit_preferences".equals(key) ||
        "AboutScreen.about_vassal".equals(key))
      return null;

    return super.addKey(key);
  }

  @Override
  public void addAction(String key, final Action action) {
    // Quit, Preferences, and About go on the special application menu

    if ("General.quit".equals(key)) {
      final Desktop app = Desktop.getDesktop();
      app.setQuitHandler(new QuitHandler() {
        @Override
        public void handleQuitRequestWith(QuitEvent e, QuitResponse resp) {
          action.actionPerformed(null);
          // the action handles exiting; if we're here, it was cancelled
          resp.cancelQuit();
        }
      });
    }
    else if ("Prefs.edit_preferences".equals(key)) {
      final Desktop app = Desktop.getDesktop();
      app.setPreferencesHandler(new PreferencesHandler() {
        @Override
        public void handlePreferences(PreferencesEvent e) {
          action.actionPerformed(null);
        }
      });
    }
    else if ("AboutScreen.about_vassal".equals(key)) {
      final Desktop app = Desktop.getDesktop();
      app.setAboutHandler(new AboutHandler() {
        @Override
        public void handleAbout(AboutEvent e) {
          action.actionPerformed(null);
        }
      });
    }
    else {
      // this is not one of the special actions
      super.addAction(key, action);
    }
  }
}
