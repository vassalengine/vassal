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

import VASSAL.Info;
import VASSAL.build.module.Documentation;
import VASSAL.configure.ShowHelpAction;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorLog;
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

    // setup menubar and actions
    final MenuManager mm = MenuManager.getInstance();
    final MenuBarProxy mb = mm.getMenuBarProxyFor(this);

    // file menu
    final MenuProxy fileMenu =
      new MenuProxy(Resources.getString("General.file"));

    fileMenu.add(mm.addMarker("PredefinedSetup.start"));
    fileMenu.add(mm.addMarker("PredefinedSetup.end"));
 
    fileMenu.add(mm.addKey("GameState.new_game"));
    fileMenu.add(mm.addKey("GameState.load_game"));
    fileMenu.add(mm.addKey("GameState.save_game"));
    fileMenu.add(mm.addKey("GameState.close_game"));
    fileMenu.addSeparator();
    fileMenu.add(mm.addKey("BasicLogger.begin_logfile"));
    fileMenu.add(mm.addKey("BasicLogger.end_logfile"));

    if (!Info.isMacOSX()) {
      fileMenu.addSeparator();
      fileMenu.add(mm.addKey("Prefs.edit_preferences"));
      fileMenu.addSeparator();
      fileMenu.add(mm.addKey("General.quit"));
    }

    mm.addAction("General.quit", new ShutDownAction());

    // help menu
    final MenuProxy helpMenu =
      new MenuProxy(Resources.getString("General.help"));
    
    helpMenu.add(mm.addKey("General.help"));

    helpMenu.addSeparator();
    helpMenu.add(mm.addMarker("Documentation.start"));
    helpMenu.add(mm.addMarker("Documentation.end"));

    helpMenu.add(mm.addKey("Documentation.about_module"));

    if (!Info.isMacOSX()) {
      helpMenu.add(mm.addKey("AboutScreen.about_vassal"));
    }

    URL url = null; 
    try {
      url = new File(Documentation.getDocumentationBaseDir(),
                     "README.html").toURI().toURL();
    }
    catch (MalformedURLException e) {
      ErrorLog.warn(e);
    }
    mm.addAction("General.help", new ShowHelpAction(url, null));

    mm.addAction("AboutScreen.about_vassal", AboutVASSAL.getAction());
    
    mb.add(fileMenu);
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
