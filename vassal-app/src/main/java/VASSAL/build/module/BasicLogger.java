/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.InputEvent;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.Info;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.Logger;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.launch.ModuleManagerUpdateHelper;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.NamedKeyStrokeListener;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.LogFileFilter;
import VASSAL.tools.io.ObfuscatingOutputStream;
import VASSAL.tools.io.ZipWriter;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.Dialogs;
import VASSAL.tools.version.VersionUtils;

/**
 * BasicLogger deals with VLOG Vassal Log files (i.e. NOT the errorLog--see below):
 * <br>(1) Starts/stops logging to a VLOG file.
 * <br>(2) Supplies prompt dialogs related to starting logfiles
 * <br>(3) Steps through VLOG files ("step forward") button
 * <br>(4) Executes the "UNDO" action when user clicks that Undo button
 * <br>(5) Provides configurers to {@link GlobalOptions} for the Undo and Step Forward buttons
 * <br><br>
 * For the errorLog file see {@link org.slf4j.Logger}, e.g.:
 * <br>org.slf4j.Logger log = LoggerFactory.getLogger(Console.class);
 * <br>log.error("write to errorlog as an error")
 * <br>log.warning("write a warning")
 * <br>log.info("write some info")
 */
public class BasicLogger implements Logger, Buildable, GameComponent, CommandEncoder {
  public static final String BEGIN = "begin_log";  //$NON-NLS-1$
  public static final String END = "end_log";  //$NON-NLS-1$
  public static final String LOG = "LOG\t";  //$NON-NLS-1$
  public static final String PROMPT_NEW_LOG = "PromptNewLog";  //$NON-NLS-1$
  public static final String PROMPT_NEW_LOG_START = "PromptNewLogAtStart"; //$NON-NLS-1$
  public static final String PROMPT_NEW_LOG_END = "PromptNewLogEnd"; //$NON-NLS-1$
  public static final String PROMPT_LOG_COMMENT = "promptLogComment";  //$NON-NLS-1$
  protected static final String STEP_ICON = "/images/StepForward16.gif";  //$NON-NLS-1$
  protected static final String UNDO_ICON = "/images/Undo16.gif";  //$NON-NLS-1$
  protected List<Command> logInput;
  protected List<Command> logOutput;
  protected int nextInput = 0;
  protected int nextUndo = -1;
  protected Command beginningState;
  protected File outputFile;
  protected Action stepAction = new StepAction();
  protected SaveMetaData metadata;
  private boolean multiPlayer = false;

  public BasicLogger() {
    super();
    stepAction.setEnabled(false);
    undoAction.setEnabled(false);
    endLogAction.setEnabled(false);
    newLogAction.setEnabled(false);
    logInput = new ArrayList<>();
    logOutput = new ArrayList<>();
  }

  /** Presently no XML attributes or subcomponents to be built */
  @Override
  public void build(Element e) { }

  /**
   * Expects to be added to a {@link GameModule}. Adds <code>Undo</code>,
   * <code>Step</code>, and <code>End Log</code> buttons to the the control
   * window toolbar. Registers {@link KeyStrokeListener}s for hotkey
   * equivalents of each button.
   */
  @Override
  public void addTo(Buildable b) {
    final GameModule mod = GameModule.getGameModule();

    mod.addCommandEncoder(this);
    mod.getGameState().addGameComponent(this);

    final MenuManager mm = MenuManager.getInstance();
    // FIXME: setting mnemonic from first letter could cause collisions in
    // some languages
    newLogAction.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("BasicLogger.begin_logfile.shortcut").charAt(0));
    mm.addAction("BasicLogger.begin_logfile", newLogAction); //NON-NLS
    // FIXME: setting mnemonic from first letter could cause collisions in
    // some languages
    endLogAction.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("BasicLogger.end_logfile.shortcut").charAt(0));
    mm.addAction("BasicLogger.end_logfile", endLogAction); //NON-NLS

    JButton button = mod.getToolBar().add(undoAction);
    button.setFocusable(false); //BR// Since for some reason we're manually making a raw "JButton" here, need to make it not focusable (so it won't start stealing keystrokes from the main window)
    button.setToolTipText(Resources.getString("BasicLogger.undo_last_move"));  //$NON-NLS-1$
    button.setAlignmentY((float) 0.0);

    button = mod.getToolBar().add(stepAction);
    button.setToolTipText(Resources.getString("BasicLogger.step_forward_tooltip"));  //$NON-NLS-1$
    button.setAlignmentY((float) 0.0);

    final NamedKeyStrokeListener undoKeyListener = new NamedKeyStrokeListener(undoAction, null);
    mod.addKeyStrokeListener(undoKeyListener);

    final NamedKeyStrokeListener stepKeyListener = new NamedKeyStrokeListener(stepAction, NamedKeyStroke.of(KeyEvent.VK_PAGE_DOWN, 0));
    mod.addKeyStrokeListener(stepKeyListener);

    final KeyStrokeListener newLogKeyListener = new KeyStrokeListener(newLogAction, KeyStroke.getKeyStroke(KeyEvent.VK_W, InputEvent.ALT_DOWN_MASK));
    mod.addKeyStrokeListener(newLogKeyListener);

    final IconConfigurer stepIconConfig = new IconConfigurer("stepIcon", Resources.getString("BasicLogger.step_forward_button"), STEP_ICON); //$NON-NLS-1$ //$NON-NLS-2$
    stepIconConfig.setValue(STEP_ICON);
    GlobalOptions.getInstance().addOption(stepIconConfig);
    stepIconConfig.addPropertyChangeListener(evt -> stepAction.putValue(Action.SMALL_ICON, stepIconConfig.getIconValue()));
    stepIconConfig.fireUpdate();

    final NamedHotKeyConfigurer stepKeyConfig = new NamedHotKeyConfigurer("stepHotKey", Resources.getString("BasicLogger.step_forward_hotkey"), stepKeyListener.getNamedKeyStroke());  //$NON-NLS-1$ //$NON-NLS-2$
    GlobalOptions.getInstance().addOption(stepKeyConfig);
    stepKeyConfig.addPropertyChangeListener(evt -> {
      stepKeyListener.setKeyStroke(stepKeyConfig.getValueNamedKeyStroke());
      if (stepKeyListener.getKeyStroke() != null) {
        stepAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString("BasicLogger.step_forward_tooltip2", NamedHotKeyConfigurer.getString(stepKeyListener.getKeyStroke())));  //$NON-NLS-1$
      }
      else {
        stepAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString("BasicLogger.step_forward_tooltip3"));  //$NON-NLS-1$
      }
    });
    stepKeyConfig.fireUpdate();

    final IconConfigurer undoIconConfig = new IconConfigurer("undoIcon", Resources.getString("BasicLogger.undo_icon"), UNDO_ICON);  //$NON-NLS-1$ //$NON-NLS-2$
    undoIconConfig.setValue(UNDO_ICON);
    GlobalOptions.getInstance().addOption(undoIconConfig);
    undoIconConfig.addPropertyChangeListener(evt -> undoAction.putValue(Action.SMALL_ICON, undoIconConfig.getIconValue()));
    undoIconConfig.fireUpdate();

    final NamedHotKeyConfigurer undoKeyConfig = new NamedHotKeyConfigurer("undoHotKey", Resources.getString("BasicLogger.undo_hotkey"), undoKeyListener.getNamedKeyStroke()); //$NON-NLS-1$ //$NON-NLS-2$
    GlobalOptions.getInstance().addOption(undoKeyConfig);
    undoKeyConfig.addPropertyChangeListener(evt -> {
      undoKeyListener.setKeyStroke(undoKeyConfig.getValueNamedKeyStroke());
      if (undoKeyListener.getKeyStroke() != null) {
        undoAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString("BasicLogger.undo_tooltip2", NamedHotKeyConfigurer.getString(undoKeyListener.getKeyStroke()))); //$NON-NLS-1$
      }
      else {
        undoAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString("BasicLogger.undo_last_move")); //$NON-NLS-1$
      }
    });
    undoKeyConfig.fireUpdate();

    final BooleanConfigurer logOptionStart = new BooleanConfigurer(PROMPT_NEW_LOG_START, Resources.getString("BasicLogger.prompt_new_log_before"), Boolean.TRUE);  //$NON-NLS-1$
    mod.getPrefs().addOption(Resources.getString("Prefs.general_tab"), logOptionStart); //$NON-NLS-1$

    final BooleanConfigurer logOptionEnd = new BooleanConfigurer(PROMPT_NEW_LOG_END, Resources.getString("BasicLogger.prompt_new_log_after"), Boolean.TRUE);  //$NON-NLS-1$
    mod.getPrefs().addOption(Resources.getString("Prefs.general_tab"), logOptionEnd); //$NON-NLS-1$

    final BooleanConfigurer logOptionComment = new BooleanConfigurer(PROMPT_LOG_COMMENT, Resources.getString("BasicLogger.enable_comments"), Boolean.TRUE);  //$NON-NLS-1$
    mod.getPrefs().addOption(Resources.getString("Prefs.general_tab"), logOptionComment); //$NON-NLS-1$
  }

  @Override
  public Element getBuildElement(Document doc) {
    return doc.createElement(getClass().getName());
  }

  @Override
  public void add(Buildable b) {
  }

  public void remove(Buildable b) {
  }

  public void setMultiPlayer(boolean multiPlayer) {
    this.multiPlayer = multiPlayer;
  }

  public boolean isMultiPlayer() {
    return multiPlayer;
  }
  
  /**
   * Our setup method is called by GameState whenever a game starts or ends.
   * @param startingGame True if a new game starting, false if a game is ending/closing
   */
  @Override
  public void setup(boolean startingGame) {
    newLogAction.setEnabled(startingGame);

    if (startingGame) {
      // When starting a game
      logOutput.clear();
      nextInput = 0;
      nextUndo = -1;
      beginningState = null; // Will create one when we actually start a log
    }
    else {
      // When ending/closing a game
      if (endLogAction.isEnabled() && (beginningState != null)) {
        if (JOptionPane.showConfirmDialog(
            GameModule.getGameModule().getPlayerWindow(),
            Resources.getString("BasicLogger.save_log"),    //$NON-NLS-1$
            Resources.getString("BasicLogger.unsaved_log"), //$NON-NLS-1$
            JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
          try {
            write();
          }
          catch (IOException e) {
            // BasicLogger is not a lumberjack
            WriteErrorDialog.error(e, outputFile);
          }
        }
      }

      logInput.clear();
      beginningState = null;
      undoAction.setEnabled(false);
      endLogAction.setEnabled(false);
      stepAction.setEnabled(false);
      outputFile = null;
    }
  }

  /** @return true if we're currently logging the game to a VLOG file */
  public boolean isLogging() {
    return outputFile != null;
  }

  /** @return true if we're currently replaying a VLOG file, that has unexecuted "step forwards" remaining */
  public boolean isReplaying() {
    return nextInput < logInput.size();
  }

  @Override
  public Command getRestoreCommand() {
    return new MultiplayerStateCommand(isMultiPlayer());
  }

  private static class MultiplayerStateCommand extends Command {
    boolean state;

    MultiplayerStateCommand(boolean state) {
      this.state = state;
    }

    @Override
    protected void executeCommand() {
      final Logger log = GameModule.getGameModule().getLogger();
      if (log instanceof BasicLogger) {
        ((BasicLogger) log).setMultiPlayer(state);
      }
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }


  public void enableDrawing(boolean show) {
  }

  /**
   * Step forward through the currently replaying vlog logfile, by getting the next {@link Command} and executing it.
   */
  protected void step() {
    if (!isReplaying()) {
      return; //BR// key held down can stack up extra calls to this in spite of "setEnabled(false)"
    }

    final GameModule g = GameModule.getGameModule();

    final Command c = logInput.get(nextInput++);
    c.execute();
    g.sendAndLog(c);
    stepAction.setEnabled(isReplaying());
    if (!isReplaying()) {
      if (GameModule.GameFileMode.REPLAYING_GAME.equals(g.getGameFileMode())) {
        g.setGameFileMode(GameModule.GameFileMode.REPLAYED_GAME);
      }
    }

    if (!isReplaying()) {
      queryNewLogFile(false);
    }
  }

  /**
   * Check if user would like to create a new logfile (only prompts if the appropriate preference is on)
   * @param atStart true if prompting because we're just starting a session; false if prompting because we just finished replaying a logfile.
   */
  public void queryNewLogFile(boolean atStart) {
    final GameModule g = GameModule.getGameModule();

    if (isLogging() || !g.getGameState().isSaveEnabled()) {
      return;
    }

    final String prefName;
    final String prompt;

    if (atStart) {
      prefName = PROMPT_NEW_LOG_START;
      prompt = Resources.getString("BasicLogger.replay_commencing");  //$NON-NLS-1$
    }
    else {
      prefName = PROMPT_NEW_LOG_END;
      prompt = Resources.getString("BasicLogger.replay_completed");  //$NON-NLS-1$
    }

    if (Boolean.TRUE.equals(g.getPrefs().getValue(prefName))) {
      final Object[] options = {
        Resources.getString(Resources.YES),
        Resources.getString(Resources.NO),
        Resources.getString("BasicLogger.dont_prompt_again")  //$NON-NLS-1$
      };

      final int result = JOptionPane.showOptionDialog(
        g.getPlayerWindow(),
        Resources.getString("BasicLogger.start_new_log_file", prompt), //$NON-NLS-1$
        "",  //$NON-NLS-1$
        JOptionPane.YES_NO_CANCEL_OPTION,
        JOptionPane.QUESTION_MESSAGE,
        null,
        options,
        options[0]
      );

      if (result == JOptionPane.YES_OPTION) {
        beginOutput();
      }
      else if (result == 2) { // Turn Preference Off
        g.getPrefs().setValue(prefName, Boolean.FALSE);
      }
    }
  }

  /**
   * Write the logfile to a file. The filename will have been selected when the logfile was begun.
   */
  public void write() throws IOException {
    if (!logOutput.isEmpty()) {
      final Command log = beginningState;
      for (final Command c : logOutput) {
        log.append(new LogCommand(c, logInput, stepAction));
      }

// FIXME: Extremely inefficient! Make encode write to an OutputStream
      final String logString = GameModule.getGameModule().encode(log);

      try (ZipWriter zw = new ZipWriter(outputFile)) {
        try (OutputStream out = new ObfuscatingOutputStream(new BufferedOutputStream(zw.write(GameState.SAVEFILE_ZIP_ENTRY)))) {
          out.write(logString.getBytes(StandardCharsets.UTF_8));
        }
        metadata.save(zw);
      }

      GameModule.getGameModule().getGameState().setModified(false);
      undoAction.setEnabled(false);
      ModuleManagerUpdateHelper.sendGameUpdate(outputFile);
    }

    endLogAction.setEnabled(false);
  }

  private File getSaveFile() {
    final GameModule g = GameModule.getGameModule();

    final FileChooser fc = g.getFileChooser();
    fc.addChoosableFileFilter(new LogFileFilter());

    String name = fc.getSelectedFile() == null
      ? null : fc.getSelectedFile().getName();
    if (name != null) {
      final int index = name.lastIndexOf('.');
      if (index > 0) {
        name = name.substring(0, index) + ".vlog";  //$NON-NLS-1$
        fc.setSelectedFile(new File(fc.getSelectedFile().getParent(), name));
      }
    }

    if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) return null;

    File file = fc.getSelectedFile();

    // append .vlog if it's not there already
    if (!file.getName().endsWith(".vlog")) {
      file = new File(file.getParent(), file.getName() + ".vlog"); //NON-NLS
    }

    // warn user if overwriting log from an old version
    if (file.exists()) {
      final AbstractMetaData md = MetaDataFactory.buildMetaData(file);
      if (md instanceof SaveMetaData) {
        if (Info.hasOldFormat(md.getVassalVersion())) {

          final int result = Dialogs.showConfirmDialog(
            g.getPlayerWindow(),
            Resources.getString("Warning.log_will_be_updated_title"),
            Resources.getString("Warning.log_will_be_updated_heading"),
            Resources.getString(
              "Warning.log_will_be_updated_message",
              file.getPath(),
              VersionUtils.truncateToMinorVersion(Info.getVersion())
            ),
            JOptionPane.WARNING_MESSAGE,
            JOptionPane.OK_CANCEL_OPTION
          );

          switch (result) {
          case JOptionPane.CANCEL_OPTION:
          case JOptionPane.CLOSED_OPTION:
            return null;
          }
        }
      }
    }

    return file;
  }

  protected void beginOutput() {
    outputFile = getSaveFile();
    if (outputFile == null) return;

    final GameModule gm = GameModule.getGameModule();

    logOutput.clear();
    beginningState = gm.getGameState().getRestoreCommand();
    if (beginningState == null) {
      return;
    }

    undoAction.setEnabled(false);
    endLogAction.setEnabled(true);
    gm.setGameFile(outputFile.getName(), GameModule.GameFileMode.LOGGING_GAME);
    GameModule.getGameModule().warn(Resources.getString("BasicLogger.logging_begun"));  //$NON-NLS-1$
    newLogAction.setEnabled(false);
    metadata = new SaveMetaData();
  }

  /**
   * This handles the UNDO button, executing the actual "Undo".
   */
  protected void undo() {
    if (nextUndo < 0) {
      return; //BR// Throw away extra keys-held-down when nothing left to do
    }

    final Command lastOutput = logOutput.get(nextUndo);
    final Command lastInput = (nextInput > logInput.size() || nextInput < 1) ?
      null : logInput.get(nextInput - 1);
    if (lastInput == lastOutput) {
      while (nextInput-- > 0) {
        stepAction.setEnabled(true);
        if (logInput.get(nextInput).getUndoCommand() != null) {
          break;
        }
      }
    }
    while (nextUndo-- > 0) {
      if (logOutput.get(nextUndo).getUndoCommand() != null) {
        break;
      }
    }
    undoAction.setEnabled(nextUndo >= 0);
    final Command undo = lastOutput.getUndoCommand();
    undo.execute();
    GameModule.getGameModule().getServer().sendToOthers(undo);
    logOutput.add(undo);
  }

  /**
   * Logs a Command to output
   * @param c Command to be logged
   */
  @Override
  public void log(Command c) {
    if (c != null && c.isLoggable()) {
      logOutput.add(c);
      if (c.getUndoCommand() != null && !c.getUndoCommand().isNull()) {
        nextUndo = logOutput.size() - 1;
      }
    }
    undoAction.setEnabled(nextUndo >= 0);
  }

  /**
   * @return true if there are Input Steps yet to be replayed
   */
  public boolean hasMoreCommands() {
    return nextInput < logInput.size();
  }

  /**
   * Recognizes a logging command. The logging command is a wrapper around an ordinary {@link Command} indicating that
   * the wrapped command should be stored and executed in sequence (when the <code>Step</code> button is pressed)
   */
  @Override
  public String encode(Command c) {
    if (!(c instanceof LogCommand)) {
      return null;
    }
    return LOG + GameModule.getGameModule().encode(((LogCommand) c).getLoggedCommand());
  }

  @Override
  public Command decode(String command) {
    if (!command.startsWith(LOG)) {
      return null;
    }

    final Command logged = GameModule.getGameModule().decode(command.substring(LOG.length()));
    if (logged == null) {
      return null;
    }

    return new LogCommand(logged, logInput, stepAction);
  }

  protected Action undoAction = new UndoAction();

  protected Action endLogAction = new AbstractAction(Resources.getString("BasicLogger.end_logfile")) {  //$NON-NLS-1$
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed(ActionEvent e) {
      try {
        if (beginningState != null) {
          write();
          GameModule.getGameModule().warn(Resources.getString("BasicLogger.logfile_written"));  //$NON-NLS-1$
          GameModule.getGameModule().setGameFileMode(GameModule.GameFileMode.LOGGED_GAME);
        }
        newLogAction.setEnabled(true);
        outputFile = null;
      }
      catch (IOException ex) {
        WriteErrorDialog.error(ex, outputFile);
      }
    }
  };

  protected Action newLogAction = new AbstractAction(Resources.getString("BasicLogger.begin_logfile")) {  //$NON-NLS-1$
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed(ActionEvent e) {
      beginOutput();
    }
  };

  public static class LogCommand extends Command {
    protected Command logged;
    protected List<Command> logInput;
    protected Action stepAction;

    public LogCommand(Command c, List<Command> logInput, Action stepAction) {
      if (c instanceof LogCommand) {
        throw new UnsupportedOperationException(
          Resources.getString("BasicLogger.cant_log"));  //$NON-NLS-1$
      }

      this.logInput = logInput;
      this.stepAction = stepAction;
      logged = c;

      for (final Command sub : c.getSubCommands()) {
        append(new LogCommand(sub, logInput, stepAction));
      }

      logged.stripSubCommands();
    }

    @Override
    protected void executeCommand() {
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }

    public Command getLoggedCommand() {
      return logged;
    }

    @Override
    public void execute() {
      final Command c = assembleCommand();
      logInput.add(c);
      stepAction.setEnabled(true);
    }

    protected Command assembleCommand() {
      final Command c = logged;
      for (final Command sub : getSubCommands()) {
        c.append(((LogCommand) sub).assembleCommand());
      }
      return c;
    }
  }

  public class StepAction extends AbstractAction {
    private static final long serialVersionUID = 1L;

    public StepAction() {
      final URL iconURL = getClass().getResource(STEP_ICON);
      if (iconURL != null) {
        putValue(Action.SMALL_ICON, new ImageIcon(iconURL));
      }
      else {
        putValue(Action.NAME, Resources.getString("BasicLogger.step"));  //$NON-NLS-1$
      }
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      step();
    }
  }
  public class UndoAction extends AbstractAction {
    private static final long serialVersionUID = 1L;

    public UndoAction() {
      final URL iconURL = getClass().getResource(UNDO_ICON);
      if (iconURL != null) {
        putValue(Action.SMALL_ICON, new ImageIcon(iconURL));
      }
      else {
        putValue(Action.NAME, Resources.getString("BasicLogger.undo"));  //$NON-NLS-1$
      }
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      undo();
    }
  }
}
