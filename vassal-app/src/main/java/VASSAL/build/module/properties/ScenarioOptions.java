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
import VASSAL.command.NullCommand;
import VASSAL.i18n.Resources;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SwingUtils;
import net.miginfocom.swing.MigLayout;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.WindowConstants;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 * A free-standing class to display the combined Scenario options
 */
public class ScenarioOptions implements GameComponent {

  private static ScenarioOptions instance;

  protected JDialog dialog;
  protected final JTabbedPane optionTabs = new JTabbedPane();
  protected Action openAction;

  /** Parent Global Properties component **/
  protected GlobalProperties globalProperties;

  public static void setInstance(ScenarioOptions options) {
    instance = options;
  }

  public static ScenarioOptions getInstance() {
    return instance;
  }

  public ScenarioOptions(GlobalProperties props) {
    setInstance(this);
    globalProperties = props;
    getDialog();
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  /**
   * Create the top-level dialog, but don't populate any tabs or options yet, this is done just before
   * display in case we are Editing and making changes, and to allow for roll-back management if user clicks cancel
   * 
   * @return generated dialog
   */
  public JDialog getDialog() {
    if (dialog == null) {
      dialog = new JDialog(GameModule.getGameModule().getPlayerWindow(), true);
      dialog.setTitle(Resources.getString("ScenarioOptions.window_title")); //$NON-NLS-1$
      dialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      dialog.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent we) {
          cancel();
        }
      });

      final JButton ok = new JButton(Resources.getString(Resources.OK));
      ok.addActionListener(e -> processChanges());

      final JButton cancel = new JButton(Resources.getString(Resources.CANCEL));
      cancel.addActionListener(e -> cancel());

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
      buttonPanel.add(cancel, "tag cancel, sg 1"); //NON-NLS
      dialog.add(buttonPanel, "grow");

      // Default actions for Enter/ESC
      SwingUtils.setDefaultButtons(dialog.getRootPane(), ok, cancel);

      openAction = new AbstractAction(Resources.getString("ScenarioOptions.menu_text")) {
        @Override
        public void actionPerformed(ActionEvent e) {
          open();
        }
      };

      MenuManager.getInstance().addAction("ScenarioOptions.menu_text", getOpenAction());
      getOpenAction().setEnabled(false);

      rebuild();
      final Dimension d = SwingUtils.getScreenSize();
      dialog.setLocation(d.width / 2 - dialog.getWidth() / 2, 0);

    }
    return dialog;
  }

  public Action getOpenAction() {
    return openAction;
  }

  /**
    * OK Button has been clicked, loop tabs and generate and combine actions for any that have changed.
    */
  protected void processChanges() {
    getDialog().setVisible(false);

    // Pause logging to collect all generated Commands into a single undo.
    final GameModule gm = GameModule.getGameModule();
    Command c = new NullCommand();
    final boolean loggingPaused = gm.pauseLogging();

    try {
      for (final ScenarioPropertiesOptionTab sptab : globalProperties.getAllDescendantComponentsOf(ScenarioPropertiesOptionTab.class)) {
        sptab.processChanges();
      }
    }
    finally {
      // In the unlikely event that logging was already paused, we do nothing, something higher up will
      // take care of it.
      if (loggingPaused) {
        c = c.append(gm.resumeLogging());
      }
    }

    // Send the accumulated logged commands.
    gm.sendAndLog(c);

  }

  /** 
   * Close button clicked. No change to underlying properties yet, so just close the dialog,
   * it will be rebuilt showing the old values when next opened.
   */ 
  protected void cancel() {
    getDialog().setVisible(false);
  }

  protected void open() {
    rebuild();
    getDialog().setVisible(true);
    SwingUtils.repack(getDialog());
  }

  // Enable the Scenario Options menu option when a game is in progress AND at least
  // one Scenario option has been defined
  @Override
  public void setup(boolean gameStarting) {
    getOpenAction().setEnabled(gameStarting && hasScenarioOptions());
  }

  // The option values are stored in their Global Properties, no restore command needed here.
  @Override
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * Return true if there is at least one ScenarioOption defined 
   */  
  protected boolean hasScenarioOptions() {
    return ! globalProperties.getAllDescendantComponentsOf(AbstractScenarioProperty.class).isEmpty();
  }

  protected void rebuild() {
    optionTabs.removeAll();

    // Loop through each defined tab in order
    for (final ScenarioPropertiesOptionTab sptab : globalProperties.getAllDescendantComponentsOf(ScenarioPropertiesOptionTab.class)) {

      String tabName = sptab.getConfigureName();
      // Force null tab name to 'General' so we get some sort of sense if designer leaves it out
      if (tabName == null || tabName.isEmpty()) {
        tabName = Resources.getString("ScenarioOptions.general_tab");
      }

      optionTabs.add(sptab.getConfigureName(), sptab.getUI());
      optionTabs.add(sptab.getConfigureName(), sptab.getUI());
    }
    dialog.pack();
  }
}
