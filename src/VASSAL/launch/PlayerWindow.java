/*
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

import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import VASSAL.tools.OrderedMenu;
import VASSAL.tools.WrapLayout;

public class PlayerWindow extends JFrame {
  private static final long serialVersionUID = 1L;

  public JToolBar getToolBar() {
    return toolBar;
  }

  public JPanel getControlPanel() {
    return controlPanel;
  }
  protected final JMenuBar menuBar = new JMenuBar();

  public JMenu getFileMenu() {
    return fileMenu;
  }

  public JMenu getHelpMenu() {
    return helpMenu;
  }

  protected final JMenu fileMenu;
  protected final JMenu helpMenu;
  protected final JToolBar toolBar = new JToolBar();
  protected final JPanel controlPanel = new JPanel();

  public PlayerWindow() {
    setTitle("VASSAL");
    setLayout(new BorderLayout());
    setJMenuBar(menuBar);

    // build File menu
    fileMenu = OrderedMenu.builder("General.file")
                          .appendItem("GameState.new_game")
                          .appendItem("GameState.load_game")
                          .appendItem("GameState.save_game")
                          .appendItem("GameState.close_game")
                          .appendSeparator()
                          .appendItem("BasicLogger.begin_logfile")
                          .appendItem("BasicLogger.end_logfile")
                          .appendSeparator()
                          .appendItem("Prefs.edit_preferences")
                          .appendSeparator()
                          .appendItem("General.quit")
                          .create();
    fileMenu.add(new ShutDownAction());
    menuBar.add(fileMenu);
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    // build Help menu
    helpMenu = OrderedMenu.builder("General.help")
                          .appendItem("About VASSAL")
                          .create();
    menuBar.add(helpMenu);
    final Action aboutVASSAL = AboutVASSAL.getAction();
    helpMenu.add(aboutVASSAL);

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
