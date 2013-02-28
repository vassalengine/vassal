/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.launch;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import org.apache.commons.lang.SystemUtils;

import VASSAL.build.module.Documentation;
import VASSAL.configure.ShowHelpAction;
import VASSAL.i18n.Resources;
import VASSAL.tools.ApplicationIcons;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.WrapLayout;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MenuProxy;

public class PlayerWindow extends JFrame {
  private static final long serialVersionUID = 1L;

  public JToolBar getToolBar() {
    return toolBar;
  }

  public JPanel getControlPanel() {
    return controlPanel;
  }

  protected final JToolBar toolBar = new JToolBar();
  protected final JPanel controlPanel = new JPanel();

  public PlayerWindow() {
    setTitle("VASSAL");
    setLayout(new BorderLayout());
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    ApplicationIcons.setFor(this);

    // setup menubar and actions
    final MenuManager mm = MenuManager.getInstance();
    final MenuBarProxy mb = mm.getMenuBarProxyFor(this);

    // file menu
    final MenuProxy fileMenu =
      new MenuProxy(Resources.getString("General.file"));
    fileMenu.setMnemonic(Resources.getString("General.file.shortcut").charAt(0));

    fileMenu.add(mm.addMarker("PredefinedSetup.start"));
    fileMenu.add(mm.addMarker("PredefinedSetup.end"));

    fileMenu.add(mm.addKey("GameState.new_game"));
    fileMenu.add(mm.addKey("GameState.load_game"));
    fileMenu.add(mm.addKey("GameState.save_game"));
    fileMenu.add(mm.addKey("GameState.close_game"));
    fileMenu.addSeparator();
    fileMenu.add(mm.addKey("BasicLogger.begin_logfile"));
    fileMenu.add(mm.addKey("BasicLogger.end_logfile"));

    if (SystemUtils.IS_OS_MAC_OSX) {
      fileMenu.add(mm.addMarker("Editor.File.start"));
      fileMenu.add(mm.addMarker("Editor.File.end"));
    }
    else {
      fileMenu.addSeparator();
      fileMenu.add(mm.addKey("Prefs.edit_preferences"));
      fileMenu.addSeparator();
      fileMenu.add(mm.addKey("General.quit"));
    }

    mm.addAction("General.quit", new ShutDownAction());

    // help menu
    final MenuProxy helpMenu =
      new MenuProxy(Resources.getString("General.help"));

    // FIMXE: setting mnemonic from first letter could cause collisions in
    // some languages
    helpMenu.setMnemonic(Resources.getString("General.help.shortcut").charAt(0));

    helpMenu.add(mm.addMarker("Documentation.VASSAL.start"));
    helpMenu.add(mm.addKey("General.help"));
    helpMenu.add(mm.addKey("Help.user_guide"));
    helpMenu.add(mm.addMarker("Documentation.VASSAL.end"));

    helpMenu.addSeparator();
    helpMenu.add(mm.addMarker("Documentation.Module.start"));
    helpMenu.add(mm.addMarker("Documentation.Module.end"));

    helpMenu.add(mm.addKey("Documentation.about_module"));

    if (!SystemUtils.IS_OS_MAC_OSX) {
      helpMenu.add(mm.addKey("AboutScreen.about_vassal"));
    }

    // Tools menu
    final MenuProxy toolsMenu =
      new MenuProxy(Resources.getString("General.tools"));
    toolsMenu.setMnemonic(Resources.getString("General.tools.shortcut").charAt(0));

    toolsMenu.add(mm.addKey("GameRefresher.refresh_counters"));

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(),
                               "README.html").toURI().toURL();
      mm.addAction("General.help", new ShowHelpAction(url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(),
                               "userguide/userguide.pdf").toURI().toURL();
      mm.addAction("Help.user_guide",
        new ShowHelpAction("Help.user_guide", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    mm.addAction("AboutScreen.about_vassal", new AboutVASSALAction(this));

    mb.add(fileMenu);
    mb.add(toolsMenu);
    mb.add(mm.addMarker("Editor.MenuBar.start"));
    mb.add(mm.addMarker("Editor.MenuBar.end"));
    mb.add(helpMenu);

    setJMenuBar(mm.getMenuBarFor(this));

    // build toolbar
    toolBar.setLayout(new WrapLayout(FlowLayout.LEFT, 0, 0));
    toolBar.setAlignmentX(0.0f);
    toolBar.setFloatable(false);
    add(toolBar, BorderLayout.NORTH);

    // build central area
    controlPanel.setLayout(new BorderLayout());
    controlPanel.setPreferredSize(new Dimension(800, 600));
    add(controlPanel, BorderLayout.CENTER);
    pack();
  }
}
