/*
 * Copyright (c) 2023 by The VASSAL Development Team
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
package VASSAL.build.module.properties;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.command.Command;
import VASSAL.configure.ComponentConfigPanel;
import VASSAL.i18n.Resources;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SwingUtils;
import net.miginfocom.swing.MigLayout;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A free-standing class to display the combined Scenario options
 */
public class ScenarioOptions implements GameComponent {

  private static ScenarioOptions instance;

  protected JDialog dialog;
  protected final JTabbedPane optionTabs = new JTabbedPane();
  protected Action openAction;

  // The individual options
  protected List<AbstractScenarioOption> options = new ArrayList<>();

  // The Named Tabs holding groups of options
  protected Map<String, ComponentConfigPanel> tabs = new HashMap<>();


  public static ScenarioOptions getInstance() {
    if (instance == null) {
      instance = new ScenarioOptions();
    }
    return instance;
  }

  public ScenarioOptions() {
    getDialog();
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  // Build the Dialog, but don't populate any tabs or options yet, this is done just before
  // display in case we are Editing and making changes
  public JDialog getDialog() {
    if (dialog == null) {
      dialog = new JDialog(GameModule.getGameModule().getPlayerWindow(), true);
      dialog.setTitle(Resources.getString("ScenarioOptions.window_title")); //$NON-NLS-1$
      dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      dialog.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent we) {
          close();
        }
      });

      final JButton ok = new JButton(Resources.getString(Resources.OK));
      ok.addActionListener(e -> close());

      dialog.setLayout(new MigLayout("insets dialog", "[push,fill]"));

      final JPanel contentPanel = new JPanel(new MigLayout("insets dialog", "[push,fill]"));
      contentPanel.add(optionTabs, "push, grow, wrap unrelated"); //NON-NLS
      final JScrollPane scrollPane = new JScrollPane(contentPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

      final JPanel scrollPanel = new JPanel(new BorderLayout());
      scrollPanel.add(scrollPane, BorderLayout.CENTER);
      dialog.add(scrollPanel, "grow, wrap");

      final JPanel buttonPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS
      buttonPanel.add(ok, "tag ok, sg 1"); //NON-NLS
      dialog.add(buttonPanel, "grow");

      // Default actions for Enter/ESC
      SwingUtils.setDefaultButtons(dialog.getRootPane(), ok, null);

      openAction = new AbstractAction(Resources.getString("ScenarioOptions.menu_text")) {
        @Override
        public void actionPerformed(ActionEvent e) {
          final Dimension d = SwingUtils.getScreenSize();
          getDialog().setLocation(d.width / 2 - getDialog().getWidth() / 2, 0);
          SwingUtils.ensureOnScreen(dialog);
          open();
        }
      };

      MenuManager.getInstance().addAction("ScenarioOptions.menu_text", getOpenAction());
      getOpenAction().setEnabled(false);

    }
    return dialog;
  }

  public Action getOpenAction() {
    return openAction;
  }

  protected void close() {
    getDialog().setVisible(false);
  }

  protected void open() {
    rebuild();
    getDialog().setVisible(true);
    SwingUtils.repack(getDialog());
  }

  protected void addOption(AbstractScenarioOption option) {
    options.add(option);
  }

  protected void removeOption(AbstractScenarioOption option) {
    options.remove(option);
  }

  // Enable the Scenario Options menu option when a game is in progress AND at least
  // one Scenario option has been defined
  @Override
  public void setup(boolean gameStarting) {
    getOpenAction().setEnabled(gameStarting && options.isEmpty());
  }

  // The option values are stored in their Global Properties, no restore command needed here.
  @Override
  public Command getRestoreCommand() {
    return null;
  }

  // How do we control the order of tabs and options?
  // They will be added in the order the Global Properties are defined.
  protected void rebuild() {
    tabs.clear();
    optionTabs.removeAll();
    for (final AbstractScenarioOption option : options) {
      String tabName = option.getTab();
      if (tabName == null || tabName.isEmpty()) {
        tabName = Resources.getString("ScenarioOptions.general_tab");
      }
      final ComponentConfigPanel tab = buildTab(tabName);
      tab.add(new JLabel(option.getPrompt()), option.getCachedConfigurer());
    }

  }

  // Find or build the required tab
  protected ComponentConfigPanel buildTab(String name) {
    ComponentConfigPanel tab = tabs.get(name);
    if (tab == null) {
      tab = new ComponentConfigPanel();
      optionTabs.addTab(name, tab);
      tabs.put(name, tab);
    }
    return tab;
  }
}
