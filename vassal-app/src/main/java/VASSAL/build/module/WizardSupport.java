/*
 *
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
package VASSAL.build.module;

import java.awt.AlphaComposite;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.ItemEvent;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.swing.Action;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.UIManager;

import net.miginfocom.swing.MigLayout;
import org.netbeans.api.wizard.WizardDisplayer;
import org.netbeans.spi.wizard.Wizard;
import org.netbeans.spi.wizard.WizardBranchController;
import org.netbeans.spi.wizard.WizardController;
import org.netbeans.spi.wizard.WizardException;
import org.netbeans.spi.wizard.WizardPage;
import org.netbeans.spi.wizard.WizardPage.WizardResultProducer;
import org.netbeans.spi.wizard.WizardPanelProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.Tutorial;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.command.Command;
import VASSAL.command.CommandFilter;
import VASSAL.command.NullCommand;
import VASSAL.counters.TraitConfigPanel;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.FileConfigurer;
import VASSAL.configure.PasswordConfigurer;
import VASSAL.configure.ShowHelpAction;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.SplashScreen;
import VASSAL.tools.image.ImageUtils;

/**
 * Provides support for two different wizards. The WelcomeWizard is the initial screen shown to the user when loading a
 * module in play mode. The GameSetupWizard is shown whenever the user starts a new game on- or off-line.
 *
 * @author rkinney
 */
public class WizardSupport {

  private static final Logger logger =
    LoggerFactory.getLogger(WizardSupport.class);

  public static final String POST_INITIAL_STEPS_WIZARD = "postInitialStepsWizard"; //$NON-NLS-1$
  public static final String POST_LOAD_GAME_WIZARD = "postLoadGameWizard"; //$NON-NLS-1$
  public static final String POST_PLAY_OFFLINE_WIZARD = "postPlayOfflineWizard"; //$NON-NLS-1$
  public static final String WELCOME_WIZARD_KEY = "welcomeWizard"; //$NON-NLS-1$
  public static final String SETUP_KEY = "setup"; //$NON-NLS-1$
  public static final String ACTION_KEY = "action"; //$NON-NLS-1$
  public static final String LOAD_TUTORIAL_ACTION = "tutorial"; //$NON-NLS-1$
  public static final String PLAY_ONLINE_ACTION = "online"; //$NON-NLS-1$
  public static final String PLAY_OFFLINE_ACTION = "offline"; //$NON-NLS-1$
  public static final String LOAD_GAME_ACTION = "loadGame"; //$NON-NLS-1$
  public static final String WELCOME_WIZARD_ENABLED = "showWelcomeWizard"; //$NON-NLS-1$
  protected Dimension logoSize = new Dimension(200, 200);
  protected List<PredefinedSetup> setups = new ArrayList<>();
  protected Tutorial tutorial;

  /**
   * Add a {@link PredefinedSetup} to the wizard page for starting a new game offline.
   *
   * Only adds setups that are not menus.
   *
   * @param setup Predefined Setup
   */
  public void addPredefinedSetup(PredefinedSetup setup) {
    if (!setup.isMenu()) {
      setups.add(setup);
    }
  }

  public void removePredefinedSetup(PredefinedSetup setup) {
    setups.remove(setup);
  }

  /**
   * Specify a {@link Tutorial} that the user may load from the {@link InitialWelcomeSteps}
   *
   * @param tutorial Tutorial
   */
  public void setTutorial(Tutorial tutorial) {
    this.tutorial = tutorial;
  }

  /**
   * Show the "Welcome" wizard, shown when loading a module in play mode
   *
   */
  public void showWelcomeWizard() {
    final GameModule g = GameModule.getGameModule();
    final WizardBranchController c = createWelcomeWizard();
    final Wizard welcomeWizard = c.createWizard();
    final HashMap<String, Wizard> props = new HashMap<>();
    props.put(WELCOME_WIZARD_KEY, welcomeWizard);

    Action help = null;
    try {
      help = new ShowHelpAction(new URL("http://www.vassalengine.org/wiki/doku.php?id=getting_started:getting_started"), null);  //NON-NLS
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    final Object result =
      WizardDisplayer.showWizard(welcomeWizard, null, help, props);

    if (result instanceof Map) {
      final Map m = (Map) result;
      final Object action = m.get(ACTION_KEY);
      if (PLAY_ONLINE_ACTION.equals(action)) {
        final ChatServerControls controls = g.getServerControls();
        g.getPlayerWindow().setVisible(true);
        controls.toggleVisible();

        new SwingWorker<Void, Void>() {
          @Override
          protected Void doInBackground() {
            controls.getClient().setConnected(true);
            return null;
          }
        }.execute();
      }
      else {
        g.getGameState().setup(true);
        g.getPlayerWindow().setVisible(true);
      }
    }
    else {
      final Boolean showWizard = (Boolean) Prefs.getGlobalPrefs().getValue(WizardSupport.WELCOME_WIZARD_KEY);
      if (Boolean.TRUE.equals(showWizard) && g.shutDown()) {
        System.exit(0);
      }
      g.getPlayerWindow().setVisible(true);
    }
  }

  protected BranchingWizard createWelcomeWizard() {
    final InitialWelcomeSteps info = createInitialWelcomeSteps();
    info.setTutorial(tutorial);
    return new BranchingWizard(info, POST_INITIAL_STEPS_WIZARD);
  }

  public WizardPanelProvider createPlayOfflinePanels() {
    if (setups.isEmpty()) {
      return GameSetupPanels.newInstance();
    }

    return new PlayOfflinePanels(
      Resources.getString("WizardSupport.WizardSupport.PlayOffline"), //$NON-NLS-1$
      Resources.getString("WizardSupport.WizardSupport.SelectSetup"), //$NON-NLS-2$
      setups);
  }

  /**
   * Show a wizard that prompts the user to specify information for unfinished {@link GameSetupStep}s
   */
  public void showGameSetupWizard() {
    final GameSetupPanels panels = GameSetupPanels.newInstance();
    if (panels != null) {
      WizardDisplayer.showWizard(panels.newWizard(logoSize), new Rectangle(0, 0, logoSize.width + 400, logoSize.height));
    }
  }


  public InitialWelcomeSteps createInitialWelcomeSteps() {
    if (!isRealName()) {
      return new InitialWelcomeSteps(new String[]{ACTION_KEY, InitialWelcomeSteps.NAME_STEP},
        new String[]{Resources.getString("WizardSupport.WizardSupport.EnterName"), Resources.getString("WizardSupport.WizardSupport.SelectPlayMode")}); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
    else {
      return new InitialWelcomeSteps(new String[]{ACTION_KEY}, new String[]{Resources.getString("WizardSupport.SelectPlayMode")}); //$NON-NLS-1$
    }
  }


  /**
   * Returns true if user has supplied a real name for current GameModule.
   *
   * Test's whether GameModule.REAL_NAME is non-empty and not "newbie"
   *
   * @return <code>true</code> if user supplied a real name
   */
  private boolean isRealName() {
    final String name = (String)GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME);
    return name != null && !name.isBlank() && !name.equals(Resources.getString("Prefs.newbie"));
  }


  /**
   * Wizard pages for the welcome wizard (initial module load). Prompts for username/password if not yet specified, and
   * prompts to load a saved game or start a new one
   *
   * @author rkinney
   *
   */
  public class InitialWelcomeSteps extends WizardPanelProvider {
    public static final String NAME_STEP = "name"; //$NON-NLS-1$
    protected JComponent nameControls;
    protected JComponent actionControls;
    protected Tutorial tutorial;

    public InitialWelcomeSteps(String[] steps, String[] stepDescriptions) {
      super(Resources.getString("WizardSupport.Welcome"), steps, stepDescriptions); //$NON-NLS-1$
    }

    @Override
    protected JComponent createPanel(WizardController controller, String id, Map settings) {
      final JComponent c;
      if (NAME_STEP.equals(id)) {
        c = getNameControls(controller, settings);

        if (settings.get(ACTION_KEY).equals(PLAY_ONLINE_ACTION)) {
          controller.setForwardNavigationMode(WizardController.MODE_CAN_FINISH);
        }
        else {
          controller.setForwardNavigationMode(WizardController.MODE_CAN_CONTINUE);
        }
      }
      else if (ACTION_KEY.equals(id)) {
        c = getActionControls(controller, settings);
      }
      else {
        throw new IllegalArgumentException("Illegal step: " + id); //$NON-NLS-1$
      }
      SplashScreen.disposeAll();
      return c;
    }

    private JComponent getActionControls(WizardController controller, final Map<String, Object> settings) {
      if (actionControls == null) {
        final Box box = Box.createVerticalBox();
        final ButtonGroup group = new ButtonGroup();
        JRadioButton tutorialButton = null;
        if (tutorial != null && tutorial.isFirstRun()) {
          tutorialButton = createTutorialButton(controller, settings);
          addButton(tutorialButton, group, box);
          box.add(Box.createVerticalStrut(20));
        }
        final JRadioButton b = createPlayOfflineButton(controller, settings);
        b.doClick();
        addButton(b, group, box);
        settings.put(ACTION_KEY, PLAY_OFFLINE_ACTION);
        addButton(createPlayOnlineButton(controller, settings), group, box);
        addButton(createLoadSavedGameButton(controller, settings), group, box);
        if (tutorialButton != null) {
          // Select tutorial button by default, but not until wizard is built.  Bug #2286742
          final JRadioButton clickOnMe = tutorialButton;
          SwingUtilities.invokeLater(clickOnMe::doClick);
          tutorialButton.addItemListener(e -> {
            if (e.getStateChange() == ItemEvent.DESELECTED) {
              tutorial.markAsViewed();
            }
          });
        }
        else if (tutorial != null) {
          addButton(createTutorialButton(controller, settings), group, box);
        }
        actionControls = box;
        box.add(Box.createVerticalGlue());
        final BooleanConfigurer wizardConf = (BooleanConfigurer)
          Prefs.getGlobalPrefs().getOption(WELCOME_WIZARD_KEY);
        final JCheckBox show = new JCheckBox(wizardConf.getName() + " " + Resources.getString("WizardSupport.Bypass"));
        show.setSelected(wizardConf.booleanValue());
        show.addActionListener(e -> wizardConf.setValue(show.isSelected()));
        box.add(show);
      }
      return actionControls;
    }

    private JRadioButton createTutorialButton(final WizardController controller, final Map<String, Object> settings) {
      final JRadioButton b = new JRadioButton(Resources.getString("WizardSupport.LoadTutorial")); //$NON-NLS-1$
      b.addActionListener(e -> {
        controller.setProblem(Resources.getString("WizardSupport.LoadingTutorial")); //$NON-NLS-1$
        try {
          new TutorialLoader(controller, settings, new BufferedInputStream(tutorial.getTutorialContents()), POST_INITIAL_STEPS_WIZARD, tutorial).start();
        }
        catch (IOException e1) {
          logger.error("", e1);
          controller.setProblem(Resources.getString("WizardSupport.ErrorLoadingTutorial")); //$NON-NLS-1$
        }
      });
      return b;
    }

    private JRadioButton createLoadSavedGameButton(final WizardController controller, final Map<String, Object> settings) {
      final JRadioButton b = new JRadioButton(Resources.getString("WizardSupport.LoadSavedGame")); //$NON-NLS-1$
      b.addActionListener(e -> {
        settings.put(WizardSupport.ACTION_KEY, LOAD_GAME_ACTION);
        final Wizard wiz = new BranchingWizard(new LoadSavedGamePanels(), POST_LOAD_GAME_WIZARD).createWizard();
        settings.put(POST_INITIAL_STEPS_WIZARD, wiz);
        controller.setForwardNavigationMode(WizardController.MODE_CAN_CONTINUE);
        controller.setProblem(null);
      });
      return b;
    }

    private JRadioButton createPlayOnlineButton(final WizardController controller, final Map<String, Object> settings) {
      final JRadioButton b = new JRadioButton(Resources.getString("WizardSupport.PlayOnline")); //$NON-NLS-1$
      b.addActionListener(e -> {
        settings.put(WizardSupport.ACTION_KEY, PLAY_ONLINE_ACTION);
        settings.put(POST_INITIAL_STEPS_WIZARD, null);
        controller.setForwardNavigationMode(isRealName() ? WizardController.MODE_CAN_FINISH : WizardController.MODE_CAN_CONTINUE);
        controller.setProblem(null);
      });
      return b;
    }

    private JRadioButton createPlayOfflineButton(final WizardController controller, final Map<String, Object> settings) {
      final JRadioButton b = new JRadioButton(Resources.getString("WizardSupport.PlayOffline")); //$NON-NLS-1$
      b.addActionListener(e -> {
        GameModule.getGameModule().getGameState().setup(false);
        GameModule.getGameModule().setGameFileMode(GameModule.GameFileMode. NEW_GAME);
        settings.put(WizardSupport.ACTION_KEY, PLAY_OFFLINE_ACTION);
        final WizardPanelProvider panels = createPlayOfflinePanels();
        if (panels == null) {
          controller.setForwardNavigationMode(WizardController.MODE_CAN_FINISH);
        }
        else {
          final Wizard wiz = new BranchingWizard(panels, POST_PLAY_OFFLINE_WIZARD).createWizard();
          settings.put(POST_INITIAL_STEPS_WIZARD, wiz);
          controller.setForwardNavigationMode(WizardController.MODE_CAN_CONTINUE);
        }
      });
      return b;
    }

    private void addButton(JRadioButton button, ButtonGroup group, Box box) {
      box.add(button);
      group.add(button);
    }

    protected JComponent getNameControls(final WizardController controller, final Map<String, Object> settings) {
      if (nameControls == null) {
        final TraitConfigPanel box = new TraitConfigPanel(); // NON-NLS
        controller.setProblem(Resources.getString("WizardSupport.EnterNameAndPassword")); //$NON-NLS-1$
        final StringConfigurer nameConfig = new StringConfigurer(null, ""); //$NON-NLS-1$
        final StringConfigurer pwd = new PasswordConfigurer(null, ""); //$NON-NLS-1$
        final StringConfigurer pwd2 = new PasswordConfigurer(null, ""); //$NON-NLS-1$
        final PropertyChangeListener pl = evt -> {
          settings.put(GameModule.REAL_NAME, nameConfig.getValue());
          settings.put(GameModule.SECRET_NAME, pwd.getValue());
          if (nameConfig.getValue() == null || "".equals(nameConfig.getValue())) { //$NON-NLS-1$
            controller.setProblem(Resources.getString("WizardSupport.EnterYourName")); //$NON-NLS-1$
          }
          else if (pwd.getValue() == null || "".equals(pwd.getValue())) { //$NON-NLS-1$
            controller.setProblem(Resources.getString("WizardSupport.EnterYourPassword")); //$NON-NLS-1$
          }
          else if (!pwd.getValue().equals(pwd2.getValue())) {
            controller.setProblem(Resources.getString("WizardSupport.PasswordsDontMatch")); //$NON-NLS-1$
          }
          else {
            final Prefs p = GameModule.getGameModule().getPrefs();

            p.getOption(GameModule.REAL_NAME).setValue(nameConfig.getValueString());

            p.getOption(GameModule.SECRET_NAME).setValue(pwd.getValueString());

            try {
              p.save();
              controller.setProblem(null);
            }
            // FIXME: review error message
            catch (IOException e) {
              String msg = e.getMessage();
              if (msg == null) {
                msg = Resources.getString("Prefs.unable_to_save");
              }
              controller.setProblem(msg);
            }
          }
        };
        nameConfig.addPropertyChangeListener(pl);
        pwd.addPropertyChangeListener(pl);
        pwd2.addPropertyChangeListener(pl);
        box.add("WizardSupport.RealName", nameConfig);
        box.add("WizardSupport.Password", pwd);
        box.add("WizardSupport.ConfirmPassword", pwd2);
        final JLabel l = new JLabel(Resources.getString("WizardSupport.NameAndPasswordDetails"));
        box.add(l, "span 2,center"); // NON-NLS
        nameControls = box;
      }
      return nameControls;
    }

    public void setTutorial(Tutorial tutorial) {
      this.tutorial = tutorial;
    }
  }
  /**
   * Wizard pages for starting a new game offline
   *
   * @author rkinney
   *
   */
  public static class PlayOfflinePanels extends WizardPanelProvider {
    private final List<PredefinedSetup> setups;
    private final String description;

    protected PlayOfflinePanels(String title, String singleDescription, List<PredefinedSetup> setups) {
      super(title, SETUP_KEY, singleDescription);
      this.setups = setups;
      this.description = singleDescription;
    }

    @Override
    protected JComponent createPanel(final WizardController controller, String id, final Map settings) {
      final DefaultComboBoxModel<Object> comboBoxModel = new DefaultComboBoxModel<>(new Vector<>(setups));
      comboBoxModel.insertElementAt(description, 0);
      final JComboBox<Object> setupSelection = new JComboBox<>(comboBoxModel);
      setupSelection.setSelectedIndex(0);
      setupSelection.addActionListener(e -> {
        if (setupSelection.getSelectedItem() instanceof PredefinedSetup) {
          final PredefinedSetup setup = (PredefinedSetup) setupSelection.getSelectedItem();
          if (setup.isUseFile() && setup.getFileName() != null) {
            loadSetup(setup, controller, settings);
          }
          else {
            final GameSetupPanels panels = GameSetupPanels.newInstance();
            settings.put(POST_PLAY_OFFLINE_WIZARD, panels);
            controller.setProblem(null);
            controller.setForwardNavigationMode(panels == null ? WizardController.MODE_CAN_FINISH : WizardController.MODE_CAN_CONTINUE);
          }
        }
        else {
          controller.setProblem(description);
        }
      });
      setupSelection.setMaximumSize(new Dimension(setupSelection.getMaximumSize().width, setupSelection.getPreferredSize().height));
      setupSelection.setRenderer(new DefaultListCellRenderer() {
        private static final long serialVersionUID = 1L;

        @Override
        public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
          final JLabel c = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
          final String labelText =
            value instanceof PredefinedSetup ?
              ((PredefinedSetup) value).getLocalizedConfigureName() :
              value == null ? "" : value.toString();
          c.setText(labelText);
          return c;
        }
      });
      final JPanel box = new JPanel(new MigLayout("ins 0", "[grow,fill]")); // NON-NLS
      box.add(setupSelection, "grow"); // NON-NLS
      controller.setProblem(description);
      return box;
    }

    protected void loadSetup(PredefinedSetup setup, final WizardController controller, final Map settings) {
      try {
        new SavedGameLoader(controller, settings, new BufferedInputStream(setup.getSavedGameContents()), POST_PLAY_OFFLINE_WIZARD).start();
      }
      // FIXME: review error message
      catch (IOException e1) {
        controller.setProblem(Resources.getString("WizardSupport.UnableToLoad"));
      }
    }
  }

  /**
   * Branches the wizard by forwarding to the Wizard stored in the wizard settings under a specified key
   */
  public static class BranchingWizard extends WizardBranchController {
    private final String wizardKey;

    public BranchingWizard(WizardPanelProvider base, String key) {
      super(base);
      this.wizardKey = key;
    }

    @Override
    protected WizardPanelProvider getPanelProviderForStep(String step, Map settings) {
      return (WizardPanelProvider) settings.get(wizardKey);
    }

    @Override
    protected Wizard getWizardForStep(String step, Map settings) {
      final Wizard w;
      final Object next = settings.get(wizardKey);
      if (next instanceof Wizard) {
        w = (Wizard) next;
      }
      else {
        w = super.getWizardForStep(step, settings);
      }
      return w;
    }
  }

  /**
   * Loads a saved game in the background. Add a branch to the wizard if the loaded game has unfinished
   * {@link GameSetupStep}s. Otherwise, enable the finish button.
   *
   * @author rkinney
   *
   */
  public static class SavedGameLoader extends Thread {
    private final WizardController controller;
    private final Map<String, Object> settings;
    // FIXME: this is a bad design---when can we safely close this stream?!
    private final InputStream in;
    private final String wizardKey;

    public SavedGameLoader(WizardController controller, Map<String, Object> settings, InputStream in, String wizardKey) {
      super();
      this.controller = controller;
      this.settings = settings;
      this.in = in;
      this.wizardKey = wizardKey;
    }

    @Override
    public void run() {
      try {
        controller.setProblem(Resources.getString("WizardSupport.LoadingGame")); //$NON-NLS-1$
        final Command setupCommand = loadSavedGame();
        setupCommand.execute();
        controller.setProblem(null);
        final GameSetupPanels panels = GameSetupPanels.newInstance();
        settings.put(wizardKey, panels);
        controller.setForwardNavigationMode(panels == null ? WizardController.MODE_CAN_FINISH : WizardController.MODE_CAN_CONTINUE);
      }
      // FIXME: review error message
      catch (IOException e) {
        controller.setProblem(Resources.getString("WizardSupport.UnableToLoad")); //$NON-NLS-1$
      }
    }

    protected Command loadSavedGame() throws IOException {
      Command setupCommand =
        GameModule.getGameModule().getGameState().decodeSavedGame(in);
      if (setupCommand == null) {
        throw new IOException(Resources.getString("WizardSupport.InvalidSavefile")); //$NON-NLS-1$
      }
      // Strip out the setup(true) command. This will be applied when the "Finish" button is pressed
      setupCommand = new CommandFilter() {
        @Override
        protected boolean accept(Command c) {
          return !(c instanceof GameState.SetupCommand) ||
            !((GameState.SetupCommand) c).isGameStarting();
        }
      }.apply(setupCommand);
      return setupCommand;
    }
  }

  public static class TutorialLoader extends SavedGameLoader {
    private final Tutorial tutorial;

    public TutorialLoader(WizardController controller, Map<String, Object> settings, InputStream in, String wizardKey, Tutorial tutorial) {
      super(controller, settings, in, wizardKey);
      this.tutorial = tutorial;
    }

    @Override
    protected Command loadSavedGame() throws IOException {
      final String msg = tutorial.getWelcomeMessage();
      Command c = msg == null ? new NullCommand() : new Chatter.DisplayText(GameModule.getGameModule().getChatter(), msg);
      c = c.append(super.loadSavedGame());
      return c;
    }
  }

  /**
   * Wizard pages for loading a saved game
   *
   * @author rkinney
   *
   */
  public static class LoadSavedGamePanels extends WizardPanelProvider {
    private FileConfigurer fileConfig;

    public LoadSavedGamePanels() {
      super(Resources.getString("WizardSupport.LoadGame"), LOAD_GAME_ACTION, Resources.getString("WizardSupport.LoadSavedGame")); //$NON-NLS-1$ //$NON-NLS-2$
    }

    @Override
    protected JComponent createPanel(final WizardController controller, String id, final Map settings) {
      if (fileConfig == null) {
        fileConfig = new FileConfigurer(null,
          Resources.getString("WizardSupport.SavedGame"), GameModule.getGameModule().getGameState().getSavedGameDirectoryPreference()); //$NON-NLS-1$
        fileConfig.addPropertyChangeListener(new PropertyChangeListener() {
          private Set<File> processing = new HashSet<>();

          @Override
          public void propertyChange(PropertyChangeEvent evt) {
            final File f = (File) evt.getNewValue();
            if (f == null || !f.exists()) {
              controller.setProblem(Resources.getString("WizardSupport.NoSuchFile")); //$NON-NLS-1$
            }
            else if (f.isDirectory()) {
              controller.setProblem(""); //$NON-NLS-1$
            }
            else if (!processing.contains(f)) { // Sometimes the FileConfigurer fires more than one event for the same
              // file
              processing.add(f);
              try {
                new SavedGameLoader(controller, settings, new BufferedInputStream(Files.newInputStream(f.toPath())), POST_LOAD_GAME_WIZARD) {
                  @Override
                  public void run() {
                    GameModule.getGameModule().getFileChooser().setSelectedFile(f); //BR// When loading a saved game from Wizard, put it appropriately into the "default" for the next save/load/etc.
                    GameModule.getGameModule().setGameFile(f.getName(), GameModule.GameFileMode.LOADED_GAME); //BR// ... aaaand put it in the app window description.
                    super.run();
                    processing.remove(f);
                  }
                }.start();
              }
              // FIXME: review error message
              catch (IOException e) {
                controller.setProblem(Resources.getString("WizardSupport.UnableToLoad")); //$NON-NLS-1$
              }
            }
          }
        });
        controller.setProblem(Resources.getString("WizardSupport.SelectSavedGame")); //$NON-NLS-1$
      }
      return (JComponent) fileConfig.getControls();
    }
  }

  /**
   * Wizard page for an unfinished {@link GameSetupStep}
   *
   * @author rkinney
   *
   */
  public static class SetupStepPage extends WizardPage {
    private static final long serialVersionUID = 1L;

    public SetupStepPage(GameSetupStep step) {
      super(step.getStepTitle());
      setLayout(new BorderLayout());
      add(step.getControls());
      putWizardData(step, step);
    }
  }

  public void setBackgroundImage(Image image) {
    if (image != null) {
      final ImageIcon icon = new ImageIcon(image);
      logoSize = new Dimension(icon.getIconWidth(), icon.getIconHeight());
      final BufferedImage img =
        ImageUtils.createCompatibleTranslucentImage(logoSize.width,
          logoSize.height);
      final Graphics2D g = img.createGraphics();
      g.setColor(Color.white);
      g.fillRect(0, 0, icon.getIconWidth(), icon.getIconHeight());
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5F));
      icon.paintIcon(null, g, 0, 0);
      g.dispose();
      UIManager.put("wizard.sidebar.image", img); //$NON-NLS-1$
    }
  }

  /**
   * Wizard pages for starting a new game. One page will be added for each unfinished {@link GameSetupStep}
   *
   * @see GameState#getUnfinishedSetupSteps()
   * @author rkinney
   */
  public static class GameSetupPanels extends WizardPanelProvider implements WizardResultProducer {
    private final WizardPage[] pages;
    private final List<GameSetupStep> setupSteps;

    private GameSetupPanels(String[] steps, String[] descriptions, WizardPage[] pages, List<GameSetupStep> setupSteps) {
      super(steps, descriptions);
      this.pages = pages;
      this.setupSteps = setupSteps;
    }

    public static GameSetupPanels newInstance() {
      GameSetupPanels panels = null;
      final ArrayList<SetupStepPage> pages = new ArrayList<>();
      final ArrayList<GameSetupStep> setupSteps = new ArrayList<>();
      for (final Iterator<GameSetupStep> i = GameModule.getGameModule().getGameState().getUnfinishedSetupSteps(); i.hasNext();) {
        final GameSetupStep step = i.next();
        setupSteps.add(step);
        final SetupStepPage page = new SetupStepPage(step);
        pages.add(page);
      }
      if (!pages.isEmpty()) {
        final WizardPage[] wizardPages = pages.toArray(new WizardPage[0]);
        final String[] steps = new String[setupSteps.size()];
        final String[] desc = new String[setupSteps.size()];
        final int n = setupSteps.size();
        for (int i = 0; i < n; i++) {
          steps[i] = String.valueOf(i);
          desc[i] = setupSteps.get(i).getStepTitle();
        }
        panels = new GameSetupPanels(steps, desc, wizardPages, setupSteps);
      }
      return panels;
    }

    @Override
    protected JComponent createPanel(WizardController controller, String id, Map settings) {
      final int index = indexOfStep(id);
      controller.setForwardNavigationMode(index == pages.length - 1 ? WizardController.MODE_CAN_FINISH : WizardController.MODE_CAN_CONTINUE);
      return pages[index];
    }

    @Override
    public boolean cancel(Map settings) {
      GameModule.getGameModule().setGameFileMode(GameModule.GameFileMode.NEW_GAME);
      GameModule.getGameModule().getGameState().setup(false);
      return true;
    }

    @Override
    public Object finish(Map wizardData) throws WizardException {
      for (final GameSetupStep step : setupSteps) {
        step.finish();
      }
      return wizardData;
    }

    public Wizard newWizard(Dimension logoSize) {
      return createWizard();
    }
  }
}
