/*
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
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.build.module.Chatter;
import VASSAL.build.module.Documentation;
import VASSAL.configure.ShowHelpAction;
import VASSAL.i18n.Resources;
import VASSAL.tools.ApplicationIcons;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.WrapLayout;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MenuProxy;
import VASSAL.tools.version.UpdateCheckAction;

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
    setTitle(Resources.getString("General.VASSAL"));
    setLayout(new BorderLayout());
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    ApplicationIcons.setFor(this);

    setupMenubarAndActions();
    buildToolbar();
    buildCentralArea();

    pack();
  }

  private void setupMenubarAndActions() {
    final MenuManager mm = MenuManager.getInstance();
    final MenuBarProxy mb = mm.getMenuBarProxyFor(this);

    // file menu
    final MenuProxy fileMenu =
      new MenuProxy(Resources.getString("General.file"));
    fileMenu.setMnemonic(Resources.getString("General.file.shortcut").charAt(0));

    fileMenu.add(mm.addMarker("PredefinedSetup.start"));  //NON-NLS
    fileMenu.add(mm.addMarker("PredefinedSetup.end"));  //NON-NLS

    fileMenu.add(mm.addKey("GameState.new_game"));
    fileMenu.add(mm.addKey("GameState.load_game_new"));
    fileMenu.add(mm.addKey("GameState.load_game_old"));
    fileMenu.add(mm.addKey("GameState.save_game"));
    fileMenu.add(mm.addKey("GameState.save_game_as"));
    fileMenu.add(mm.addKey("GameState.close_game"));
    fileMenu.addSeparator();
    fileMenu.add(mm.addKey("BasicLogger.begin_logfile"));
    fileMenu.add(mm.addKey("BasicLogger.end_logfile"));

    if (SystemUtils.IS_OS_MAC) {
      fileMenu.add(mm.addMarker("Editor.File.start"));  //NON-NLS
      fileMenu.add(mm.addMarker("Editor.File.end"));  //NON-NLS
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

    // FIXME: setting mnemonic from first letter could cause collisions in some languages
    helpMenu.setMnemonic(Resources.getString("General.help.shortcut").charAt(0));

    helpMenu.add(mm.addMarker("Documentation.VASSAL.start"));  //NON-NLS
    helpMenu.add(mm.addKey("General.help"));
    helpMenu.add(mm.addKey("Help.user_guide"));
    helpMenu.add(mm.addMarker("Documentation.VASSAL.end"));  //NON-NLS

    helpMenu.addSeparator();
    helpMenu.add(mm.addMarker("Documentation.Module.start"));  //NON-NLS
    helpMenu.add(mm.addMarker("Documentation.Module.end"));  //NON-NLS

    helpMenu.add(mm.addKey("Documentation.about_module"));

    helpMenu.addSeparator();

    helpMenu.add(mm.addKey("UpdateCheckAction.update_check"));
    helpMenu.add(mm.addKey("Help.error_log"));

    if (!SystemUtils.IS_OS_MAC) {
      helpMenu.add(mm.addKey("AboutScreen.about_vassal"));
    }

    // Tools menu
    final MenuProxy toolsMenu =
      new MenuProxy(Resources.getString("General.tools"));
    toolsMenu.setMnemonic(Resources.getString("General.tools.shortcut").charAt(0));

    toolsMenu.add(mm.addKey("GameRefresher.refresh_counters"));
    toolsMenu.add(mm.addKey("GameState.load_continuation"));

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

    mm.addAction("UpdateCheckAction.update_check", new UpdateCheckAction(this));
    mm.addAction("Help.error_log", new ShowErrorLogAction(this));

    mb.add(fileMenu);
    mb.add(toolsMenu);
    mb.add(mm.addMarker("Editor.MenuBar.start"));  //NON-NLS
    mb.add(mm.addMarker("Editor.MenuBar.end"));  //NON-NLS
    mb.add(helpMenu);

    setJMenuBar(mm.getMenuBarFor(this));
  }

  private void buildToolbar() {
    toolBar.setLayout(new WrapLayout(WrapLayout.LEFT, 0, 0));
    toolBar.setAlignmentX(0.0f);
    toolBar.setFloatable(false);
    add(toolBar, BorderLayout.NORTH);
  }

  private void buildCentralArea() {
    controlPanel.setLayout(new BorderLayout());
    controlPanel.setPreferredSize(new Dimension(800, 600));
    add(controlPanel, BorderLayout.CENTER);
  }

  /**
   * Create a new hideable panel beside the control panel.
   *
   * @param newComponent the hideable component
   * @param hideablePosition the position of the hideable component, one of
   * {@link ComponentSplitter.SplitPane#HIDE_TOP}, {@link ComponentSplitter.SplitPane#HIDE_RIGHT},
   * {@link ComponentSplitter.SplitPane#HIDE_BOTTOM}, {@link ComponentSplitter.SplitPane#HIDE_LEFT}
   * @param resize If true, the containing window will expand or shrink to an appropriate size when
   *     the hideable component is shown or hidden
   * @return the {@link ComponentSplitter.SplitPane} containing the two components
   *
   */
  @SuppressWarnings("removal")
  @Deprecated(since = "2020-07-28", forRemoval = true)
  public ComponentSplitter.SplitPane splitControlPanel(Component newComponent, int hideablePosition, boolean resize) {
    int index = -1;
    final Container parent = controlPanel.getParent();
    if (parent != null) {
      final int n = parent.getComponentCount();
      for (int i = 0; i < n; ++i) {
        if (controlPanel == parent.getComponent(i)) {
          index = i;
          break;
        }
      }
    }

    final ComponentSplitter.SplitPane
      split = new ComponentSplitter.SplitPane(newComponent, controlPanel, hideablePosition, resize);
    if (index >= 0) {
      parent.add(split, index);
    }
    return split;
  }

  public void addChatter(Chatter chatter) {
    controlPanel.add(chatter, BorderLayout.CENTER);
  }
}
