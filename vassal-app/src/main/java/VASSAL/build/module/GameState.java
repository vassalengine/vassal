/*
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import VASSAL.preferences.Prefs;
import VASSAL.tools.ProblemDialog;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;
import javax.swing.SwingUtilities;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;

import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.command.AddPiece;
import VASSAL.command.AlertCommand;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.CommandFilter;
import VASSAL.command.ConditionalCommand;
import VASSAL.command.Logger;
import VASSAL.command.NullCommand;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.launch.ModuleManagerUpdateHelper;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.WarningDialog;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.LogAndSaveFileFilter;
import VASSAL.tools.io.DeobfuscatingInputStream;
import VASSAL.tools.io.ObfuscatingOutputStream;
import VASSAL.tools.io.ZipWriter;
import VASSAL.tools.io.ZipArchive;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.Dialogs;
import VASSAL.tools.version.VersionUtils;

/**
 * The GameState contains methods to track and read/write the complete enumerated game state of the game
 * currently being played. Its main methods deal with saving/loading games and starting/ending games: see
 * {@link #saveGameAs()}, {@link #loadGame()}, {@link #setup(boolean)}. In each case, appropriate calls
 * are made to designated methods in all relevant GameComponents. Can also be queried if a game is in progress
 * {@link #isGameStarted()} and if the game state has been modified since the last save {@link #isModified()}.
 *
 * Only one game can be open at once in a single Player, or in a single Editor/Player pair.
 *
 * @see GameModule#getGameState
 */
public class GameState implements CommandEncoder {
  private static final org.slf4j.Logger log =
    LoggerFactory.getLogger(GameState.class);

  protected Map<String, GamePiece> pieces = new HashMap<>();
  protected List<GameComponent> gameComponents = new ArrayList<>();
  protected List<GameSetupStep> setupSteps = new ArrayList<>();
  protected Action loadGame, loadGameOld, saveGame, saveGameAs, newGame, closeGame, loadContinuation;
  protected String lastSave;
  protected File lastSaveFile = null;
  protected DirectoryConfigurer savedGameDirectoryPreference;
  protected String loadComments;

  //public GameState() {}

  /**
   * Expects to be added to a GameModule.  Adds <code>New</code>,
   * <code>Load</code>, <code>Close</code>, and <code>Save</code>
   * entries to the <code>File</code> menu of the controls window
   */
  public void addTo(@SuppressWarnings("unused") GameModule mod) {
    loadGame = new AbstractAction(Resources.getString("GameState.load_game_new")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        loadGame(false);
      }
    };
    // FIXME: setting mnemonic from first letter could cause collisions in
    // some languages
    loadGame.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.load_game.shortcut").charAt(0));

    // TODO: remove Load Continuation from File menu as of 3.6
    loadGameOld = new AbstractAction(Resources.getString("GameState.load_continuation")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        ProblemDialog.show(
          JOptionPane.INFORMATION_MESSAGE,
          GameModule.getGameModule().getPlayerWindow(),
          null,
          Resources.getString("GameState.old_continuation_title"),
          Resources.getString("GameState.old_continuation_heading"),
          Resources.getString("GameState.old_continuation_warning")
        );
      }
    };
    loadGameOld.setEnabled(false);

    loadContinuation = new AbstractAction(Resources.getString("GameState.load_game_old")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        loadGame(true);
      }
    };
    loadContinuation.setEnabled(false);

    saveGame = new AbstractAction(Resources.getString("GameState.save_game")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        saveGame();
      }
    };
    // FIXME: setting mnemonic from first letter could cause collisions in
    // some languages
    saveGame.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.save_game.shortcut").charAt(0));

    saveGameAs = new AbstractAction(Resources.getString("GameState.save_game_as")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        saveGameAs();
      }
    };
    // FIXME: setting mnemonic from first letter could cause collisions in
    // some languages
    saveGameAs.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.save_game_as.shortcut").charAt(0));

    newGame = new AbstractAction(Resources.getString("GameState.new_game")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        GameModule.getGameModule().setGameFileMode(GameModule.GameFileMode.NEW_GAME);
        setup(false);

        final Logger log = GameModule.getGameModule().getLogger();
        if (log instanceof BasicLogger) {
          ((BasicLogger)log).setMultiPlayer(GameModule.getGameModule().isMultiPlayer());
        }

        setup(true);
      }
    };
    // FIXME: setting mnemonic from first letter could cause collisions in
    // some languages
    newGame.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.new_game.shortcut").charAt(0));

    closeGame = new AbstractAction(
        Resources.getString("GameState.close_game")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        closeGame();
      }
    };
    // FIXME: setting mnemonic from first letter could cause collisions in
    // some languages
    closeGame.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.close_game.shortcut").charAt(0));

    final MenuManager mm = MenuManager.getInstance();
    mm.addAction("GameState.new_game", newGame);
    mm.addAction("GameState.load_game_new", loadGame);
    mm.addAction("GameState.load_game_old", loadGameOld);
    mm.addAction("GameState.load_continuation", loadContinuation);
    mm.addAction("GameState.save_game", saveGame);
    mm.addAction("GameState.save_game_as", saveGameAs);
    mm.addAction("GameState.close_game", closeGame);

    saveGame.setEnabled(gameStarting);
    saveGameAs.setEnabled(gameStarting);
    closeGame.setEnabled(gameStarting);
    loadGameOld.setEnabled(gameStarting);
    loadContinuation.setEnabled(gameStarting);
  }

  /**
   * @return true if the game state is different from when it was last saved
   */
  public boolean isModified() {
    final String s = saveString();
    return s != null && !s.equals(lastSave);
  }


  /**
   * @return true if saveGame action is enabled (mainly to detect if logging can start)
   */
  public boolean isSaveEnabled() {
    return saveGame != null && saveGame.isEnabled();
  }

  /**
   * Add a {@link GameComponent} to the list of objects that will
   * be notified when a game is started/ended
   */
  public void addGameComponent(GameComponent theComponent) {
    gameComponents.add(theComponent);
  }

  /**
   * Remove a {@link GameComponent} from the list of objects that will
   * be notified when a game is started/ended
   */
  public void removeGameComponent(GameComponent theComponent) {
    gameComponents.remove(theComponent);
  }

  /**
   * @return an enumeration of all {@link GameComponent} objects
   * that have been added to this GameState
   * @deprecated Use {@link #getGameComponents()} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Enumeration<GameComponent> getGameComponentsEnum() {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
    return Collections.enumeration(gameComponents);
  }

  /**
   * @return a Collection of all {@link GameComponent} objects
   * that have been added to this GameState
   */
  public Collection<GameComponent> getGameComponents() {
    return Collections.unmodifiableCollection(gameComponents);
  }

  /** Add a {@link GameSetupStep} */
  public void addGameSetupStep(GameSetupStep step) {
    setupSteps.add(step);
  }

  /** Remove a {@link GameSetupStep} */
  public void removeGameSetupStep(GameSetupStep step) {
    setupSteps.remove(step);
  }

  /**
   * @return an iterator of all {@link GameSetupStep}s that are not
   * yet finished
   */
  public Iterator<GameSetupStep> getUnfinishedSetupSteps() {
    final ArrayList<GameSetupStep> l = new ArrayList<>();
    for (final GameSetupStep step : setupSteps) {
      if (!step.isFinished()) {
        l.add(step);
      }
    }
    return l.iterator();
  }

  /* Using an instance variable allows us to shut down in the
     middle of a startup. */
  private boolean gameStarting = false;
  private boolean gameStarted = false;

  //
  // FIXME: This will become unnecessary when we do model-view separation.
  //
  private volatile boolean gameUpdating = false;

  public void setupRefresh() {
    this.gameStarting = false;
    newGame.setEnabled(false);
    saveGame.setEnabled(true);
    saveGameAs.setEnabled(true);
    closeGame.setEnabled(true);

    gameStarted &= this.gameStarting;
    for (final GameComponent gc : gameComponents) {
      gc.setup(this.gameStarting);
    }
  }

  /**
   * Start a game for updating (via editor).
   * <em>NOTE: This method is not for use in custom code.</em>
   */
  public void setup(boolean gameStarting, boolean gameUpdating) {
    GameModule.getGameModule().setGameFileMode(GameModule.GameFileMode.NEW_GAME);
    this.gameUpdating = gameUpdating;
    setup(gameStarting);
  }

  /**
   * Indicated game update is completed and game is saved.
   */
  public void updateDone() {
    this.gameUpdating = false;
  }

  public boolean isUpdating() {
    return this.gameUpdating;
  }
  //
  // END FIXME
  //

  /**
   * Start/end a game.  Prompt to save if the game state has been
   * modified since last save.  Invoke {@link GameComponent#setup}
   * on all registered {@link GameComponent} objects.
   */
  public void setup(boolean gameStarting) {
    final GameModule g = GameModule.getGameModule();

    if (g.isRefreshingSemaphore()) {
      return; // Blocks setup method during Game Refresh
    }

    if (!gameStarting && gameStarted && isModified()) {
      switch (JOptionPane.showConfirmDialog(
        g.getPlayerWindow(),
        Resources.getString("GameState.save_game_query"), //$NON-NLS-1$
        Resources.getString("GameState.game_modified"),   //$NON-NLS-1$
        JOptionPane.YES_NO_CANCEL_OPTION)) {
      case JOptionPane.YES_OPTION:
        saveGame();
        break;
      case JOptionPane.CANCEL_OPTION:
      case JOptionPane.CLOSED_OPTION:
        return;
      }
    }

    this.gameStarting = gameStarting;

    if (!gameStarting) {
      pieces.clear();
    }

    newGame.setEnabled(!gameStarting);
    saveGame.setEnabled(gameStarting);
    saveGameAs.setEnabled(gameStarting);
    closeGame.setEnabled(gameStarting);

    g.resetSourcesAndListeners();

    if (gameStarting) {
      g.getWizardSupport().showGameSetupWizard();
    }

    loadGameOld.setEnabled(gameStarting);
    loadContinuation.setEnabled(gameStarting);

    gameStarted &= this.gameStarting;
    for (final GameComponent gc : gameComponents) {
      gc.setup(this.gameStarting);
    }

    gameStarted |= this.gameStarting;
    lastSave = gameStarting ? saveString() : null;
    lastSaveFile = null;

    if (gameStarted) {
      if (gameStarting) {
        // If we're starting a new session, prompt to create a new logfile.
        // But NOT if we're starting a session by *replaying* a logfile -- in that case we'd get the reminder at the
        // end of the logfile.
        SwingUtilities.invokeLater(() -> {
          final Logger logger = GameModule.getGameModule().getLogger();
          if (logger instanceof BasicLogger) {
            if (!((BasicLogger)logger).isReplaying()) {
              ((BasicLogger) logger).queryNewLogFile(true);
            }
          }
        });
      }
    }
  }

  /** Return true if a game is currently in progress */
  public boolean isGameStarted() {
    return gameStarted;
  }

  /**
   * Read the game from a savefile.  The contents of the file is
   * sent to {@link GameModule#decode} and translated into a
   * {@link Command}, which is then executed.  The command read from the
   * file should be that returned by {@link #getRestoreCommand}.
   *
   * This version for binary compatibility w/ old style.
   */
  public void loadGame() {
    loadGame(true);
  }

  /**
   * Read the game from a savefile.  The contents of the file is
   * sent to {@link GameModule#decode} and translated into a
   * {@link Command}, which is then executed.  The command read from the
   * file should be that returned by {@link #getRestoreCommand}.
   *
   * @param continuation if true then do the "old-style" version of load continuation
   */
  public void loadGame(boolean continuation) {
    final GameModule g = GameModule.getGameModule();

    if (gameStarted && continuation) {
      if (GlobalOptions.getInstance().isWarnOldContinuation()) {
        final Object[] options = {
          Resources.getString("GameState.anyway"),
          Resources.getString("GameState.cancel"),
          Resources.getString("GameState.dont_prompt_again")
        };

        final int result = JOptionPane.showOptionDialog(
          g.getPlayerWindow(),
          Resources.getString("GameState.old_continuation_warning"),
          "",
          JOptionPane.YES_NO_CANCEL_OPTION,
          JOptionPane.QUESTION_MESSAGE,
          null,
          options,
          options[0]
        );
        if (result == JOptionPane.NO_OPTION) { // Note - this is actually the "Cancel" option.
          return;
        }
        else if (result == JOptionPane.CANCEL_OPTION) { // "Don't Prompt Again" option.
          g.getPrefs().setValue(GlobalOptions.OLD_CONTINUATION, Boolean.FALSE);
        }
      }
    }

    loadComments = "";
    final FileChooser fc = g.getFileChooser();
    fc.addChoosableFileFilter(new LogAndSaveFileFilter());

    if (fc.showOpenDialog() != FileChooser.APPROVE_OPTION) return;

    final File f = fc.getSelectedFile();
    try {
      if (!f.exists()) throw new FileNotFoundException(
        "Unable to locate " + f.getPath()); //NON-NLS

      // Check the Save game for validity
      final AbstractMetaData metaData = MetaDataFactory.buildMetaData(f);
      if (!(metaData instanceof SaveMetaData)) {
        WarningDialog.show("GameState.invalid_save_file", f.getPath()); //NON-NLS
        return;
      }

      // Check it belongs to this module and matches the version if is a
      // post 3.0 save file
      final SaveMetaData saveData = (SaveMetaData) metaData;
      String saveModuleVersion = "?";
      if (saveData.getModuleData() != null) {
        loadComments = saveData.getLocalizedDescription();
        final String saveModuleName = saveData.getModuleName();
        saveModuleVersion = saveData.getModuleVersion();
        final String moduleName = g.getGameName();
        final String moduleVersion = g.getGameVersion();
        String message = null;

        if (!saveModuleName.equals(moduleName)) {
          message = Resources.getString(
            "GameState.load_module_mismatch",
            f.getName(), saveModuleName, moduleName
          );
        }
        else if (!saveModuleVersion.equals(moduleVersion)) {
          message = Resources.getString(
            "GameState.load_version_mismatch",
            f.getName(), saveModuleVersion, moduleVersion
          );
        }

        if (message != null) {
          if (JOptionPane.showConfirmDialog(
              null,
              message,
              Resources.getString("GameState.load_mismatch"),
              JOptionPane.YES_NO_OPTION,
              JOptionPane.QUESTION_MESSAGE) != JOptionPane.YES_OPTION) {
            g.warn(Resources.getString("GameState.cancel_load", f.getName()));
            return;
          }
        }
      }

      log.info(
        "Loading save game " + f.getPath() + //NON-NLS
        ", created with module version " + saveModuleVersion //NON-NLS
      );

      if (gameStarted && continuation) {
        loadContinuation(f);
      }
      else {
        g.setGameFile(f.getName(), GameModule.GameFileMode.LOADED_GAME);

        //BR// New preferred style load for vlogs is close the old stuff and hard-reset to the new log state.
        if (gameStarted) {
          GameModule.getGameModule().setGameFileMode(GameModule.GameFileMode.NEW_GAME);
          
          GameModule.getGameModule().setLoadOverSemaphore(true); // Stop updating Map UI etc for a bit
          try {
            loadGameInForeground(f); // Foreground loading minimizes the bad behavior of windows during vlog load "mid game"
          }
          finally {
            GameModule.getGameModule().setLoadOverSemaphore(false); // Resume normal UI updates
          }
        }
        else {
          loadGameInBackground(f);
        }
      }

      lastSaveFile = f;
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, f);
    }
  }

  protected String saveString() {
    return GameModule.getGameModule().encode(getRestoreCommand());
  }

  protected boolean checkForOldSaveFile(File f) {
    if (f.exists()) {
      // warn user if overwriting a save from an old version
      final AbstractMetaData md = MetaDataFactory.buildMetaData(f);
      if (md instanceof SaveMetaData) {
        if (Info.hasOldFormat(md.getVassalVersion())) {
          return Dialogs.showConfirmDialog(
            GameModule.getGameModule().getPlayerWindow(),
            Resources.getString("Warning.save_will_be_updated_title"),
            Resources.getString("Warning.save_will_be_updated_heading"),
            Resources.getString(
              "Warning.save_will_be_updated_message",
              f.getPath(),
              VersionUtils.truncateToMinorVersion(Info.getVersion())
            ),
              JOptionPane.WARNING_MESSAGE,
            JOptionPane.OK_CANCEL_OPTION) != JOptionPane.CANCEL_OPTION;
        }
      }
    }

    return true;
  }


  /** Closes the game. */
  public void closeGame() {
    GameModule.getGameModule().setGameFileMode(GameModule.GameFileMode.NEW_GAME);
    setup(false);
  }

  /** Saves the game to an existing file, or prompts for a new one. */
  public void saveGame() {
    if (lastSaveFile != null) {
      if (!checkForOldSaveFile(lastSaveFile)) {
        return;
      }

      try {
        saveGame(lastSaveFile);
        if (!GameModule.getGameModule().isReplayingOrLogging()) {
          GameModule.getGameModule().setGameFile(lastSaveFile.getName(), GameModule.GameFileMode.SAVED_GAME);
        }
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, lastSaveFile);
      }
    }
    else {
      saveGameAs();
    }
  }

  /** Prompts the user for a file into which to save the game */
  public void saveGameAs() {
    final GameModule g = GameModule.getGameModule();

    final File saveFile = getSaveFile();
    if (saveFile == null) {
      g.warn(Resources.getString("GameState.save_canceled"));  //$NON-NLS-1$
    }
    else {
      if (!checkForOldSaveFile(saveFile)) {
        return;
      }

      try {
        saveGame(saveFile);
        lastSaveFile = saveFile;
        if (!GameModule.getGameModule().isReplayingOrLogging()) {
          g.setGameFile(saveFile.getName(), GameModule.GameFileMode.SAVED_GAME);
        }
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, saveFile);
      }
    }
  }

  public void setModified(boolean modified) {
    if (modified) {
      lastSave = null;
    }
    else {
      lastSave = saveString();
    }
  }

  private File getSaveFile() {
    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    fc.selectDotSavFile();
    fc.addChoosableFileFilter(new LogAndSaveFileFilter());

    if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) return null;

    File file = fc.getSelectedFile();

    // append .vsav if it's not there already
    if (!file.getName().endsWith(".vsav")) {
      file = new File(file.getParent(), file.getName() + ".vsav"); //NON-NLS
    }

    return file;
  }

  /**
   *  Add a {@link GamePiece} to the current game.
   * The GameState keeps track of all GamePieces in the system,
   * regardless of which {@link Map} they belong to (if any)
   */
  public void addPiece(GamePiece p) {
    if (p.getId() == null) {
      p.setId(getNewPieceId());
    }
    pieces.put(p.getId(), p);
  }

  /**
   * @return the {@link GamePiece} in the current game with the given id
   */
  public GamePiece getPieceForId(String id) {
    return id == null ? null : pieces.get(id);
  }

  /**
   *  Remove a {@link GamePiece} from the current game
   */
  public void removePiece(String id) {
    if (id != null) {
      pieces.remove(id);
    }
  }

  /**
   * @return a String identifier guaranteed to be different from the
   * id of all GamePieces in the game
   *
   * @see GamePiece#getId
   */
  public String getNewPieceId() {
    long time = System.currentTimeMillis();
    String id = Long.toString(time);
    while (pieces.get(id) != null) {
      id = Long.toString(++time);
    }
    return id;
  }

  public void loadContinuation(File f) throws IOException {
    final GameModule g = GameModule.getGameModule();
    g.warn(Resources.getString("GameState.loading", f.getName()));  //$NON-NLS-1$

    Command c;

    g.setLoadingContinuationSemaphore(true); //BR// So we won't thrash the listeners while decoding.
    try {
      c = decodeSavedGame(f);
    }
    finally {
      g.setLoadingContinuationSemaphore(false);
    }

    final CommandFilter filter = new CommandFilter() {
      @Override
      protected boolean accept(Command c) {
        return c instanceof BasicLogger.LogCommand;
      }
    };
    c = filter.apply(c);
    if (c != null) {
      c.execute();
    }
    g.setGameFile(f.getName(), ((BasicLogger)g.getLogger()).isReplaying() ? GameModule.GameFileMode.REPLAYING_GAME : GameModule.GameFileMode.LOADED_GAME);
    String msg = Resources.getString("GameState.loaded", f.getName());  //$NON-NLS-1$
    if (loadComments != null && loadComments.length() > 0) {
      msg = "!" + msg + ": <b>" + loadComments + "</b>"; //$NON-NLS-1$
    }
    GameModule.getGameModule().warn(msg);
  }

  /**
   * @return an Enumeration of all {@link GamePiece}s in the game
   * @deprecated Use {@link #getAllPieces()} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Enumeration<GamePiece> getPieces() {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
    return Collections.enumeration(pieces.values());
  }

  /** @return a Collection of all {@link GamePiece}s in the game */
  public Collection<GamePiece> getAllPieces() {
    return pieces.values();
  }

  public static class SetupCommand extends Command {
    private final boolean gameStarting;

    public SetupCommand(boolean gameStarting) {
      this.gameStarting = gameStarting;
    }

    public boolean isGameStarting() {
      return gameStarting;
    }

    @Override
    protected void executeCommand() {
      GameModule.getGameModule().getGameState().setup(gameStarting);
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }

  public static final String SAVEFILE_ZIP_ENTRY = "savedGame";  //$NON-NLS-1$

  /**
   * @return a {@link Command} that, when executed, will restore the
   * game to its current state -- this command can then be written to
   * a save game file. Invokes {@link GameComponent#getRestoreCommand}
   * on each registered {@link GameComponent}, and then creates an AddPiece command
   * for each game piece.
   */
  public Command getRestoreCommand() {
    if (!saveGame.isEnabled()) {
      return null;
    }
    final Command c = new SetupCommand(false);
    c.append(checkVersionCommand());
    c.append(getRestorePiecesCommand());
    for (final GameComponent gc : gameComponents) {
      c.append(gc.getRestoreCommand());
    }
    c.append(new SetupCommand(true));
    return c;
  }

  private Command checkVersionCommand() {
    final String runningVersion = GameModule.getGameModule().getAttributeValueString(GameModule.VASSAL_VERSION_RUNNING);
    ConditionalCommand.Condition cond = new ConditionalCommand.Lt(GameModule.VASSAL_VERSION_RUNNING, runningVersion);
    final Command c = new ConditionalCommand(new ConditionalCommand.Condition[]{cond}, new AlertCommand(Resources.getString("GameState.version_mismatch", runningVersion)));  //$NON-NLS-1$
    final String moduleName = GameModule.getGameModule().getAttributeValueString(GameModule.MODULE_NAME);
    final String moduleVersion = GameModule.getGameModule().getAttributeValueString(GameModule.MODULE_VERSION);
    cond = new ConditionalCommand.Lt(GameModule.MODULE_VERSION, moduleVersion);
    c.append(
      new ConditionalCommand(
        new ConditionalCommand.Condition[]{cond},
        new AlertCommand(Resources.getString("GameState.version_mismatch2", moduleName, moduleVersion))));  //$NON-NLS-1$
    return c;
  }

  /**
   * A GameState recognizes instances of {@link SetupCommand}
   */
  @Override
  public String encode(Command c) {
    if (!(c instanceof SetupCommand)) {
      return null;
    }
    return ((SetupCommand) c).isGameStarting() ? END_SAVE : BEGIN_SAVE;
  }

  /**
   * A GameState recognizes instances of {@link SetupCommand}
   */
  @Override
  public Command decode(String theCommand) {
    if (BEGIN_SAVE.equals(theCommand)) {
      return new SetupCommand(false);
    }

    if (END_SAVE.equals(theCommand)) {
      return new SetupCommand(true);
    }

    return null;
  }
  public static final String BEGIN_SAVE = "begin_save";  //$NON-NLS-1$
  public static final String END_SAVE = "end_save";  //$NON-NLS-1$

  public void saveGameRefresh(ZipArchive archive) throws IOException {
    final SaveMetaData metaData;
    // FIXME: It is extremely inefficient to produce the save string. It would
    // be faster to write directly to the output stream instead.

    // store the prompt pref
    final GameModule mod = GameModule.getGameModule();
    final Prefs myPrefs = mod.getPrefs();
    final Boolean oldPrompt = (Boolean)myPrefs.getValue(SaveMetaData.PROMPT_LOG_COMMENT);

    // turn off prompting for the save
    myPrefs.setValue(SaveMetaData.PROMPT_LOG_COMMENT, false);
    metaData = new SaveMetaData(); // this also potentially prompts for save file comments, so do *before* possibly long save file write

    final String save = saveString();
    try (OutputStream zout = archive.getOutputStream(SAVEFILE_ZIP_ENTRY);
         BufferedOutputStream bout = new BufferedOutputStream(zout);
         OutputStream out = new ObfuscatingOutputStream(bout)) {
      out.write(save.getBytes(StandardCharsets.UTF_8));
    }
    archive.close();

    metaData.save(archive);
    // reset the pref
    myPrefs.setValue(SaveMetaData.PROMPT_LOG_COMMENT, oldPrompt);
  }

  public void saveGame(File f) throws IOException {
    final SaveMetaData metaData;
    GameModule.getGameModule().warn(Resources.getString("GameState.saving_game") + ": " + f.getName());  //$NON-NLS-1$
    // FIXME: It is extremely inefficient to produce the save string. It would
    // be faster to write directly to the output stream instead.
    metaData = new SaveMetaData(); // this also potentially prompts for save file comments, so do *before* possibly long save file write

    final String save = saveString();

    try (ZipWriter zw = new ZipWriter(f)) {
      try (OutputStream out = new ObfuscatingOutputStream(new BufferedOutputStream(zw.write(SAVEFILE_ZIP_ENTRY)))) {
        out.write(save.getBytes(StandardCharsets.UTF_8));
      }
      metaData.save(zw);
    }

    lastSave = save;
    final String msg;
    final String saveComments = metaData.getLocalizedDescription();
    if (!StringUtils.isEmpty(saveComments)) {
      msg = "!" + Resources.getString("GameState.game_saved") + ": <b>" + saveComments + "</b>"; //$NON-NLS-1$
    }
    else {
      msg = Resources.getString("GameState.game_saved"); //$NON-NLS-1$
    }
    GameModule.getGameModule().warn(msg);
    ModuleManagerUpdateHelper.sendGameUpdate(f);
  }


  public void loadGameInForeground(final File f) {
    try {
      loadGameInForeground(
        f.getName(),
        new BufferedInputStream(Files.newInputStream(f.toPath()))
      );
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, f);
    }
  }

  public void loadGameInForeground(final String shortName,
                                   final InputStream in) throws IOException {
    GameModule.getGameModule().warn(
      Resources.getString("GameState.loading", shortName));  //$NON-NLS-1$

    final Command loadCommand = decodeSavedGame(in);
    if (loadCommand != null) {
      try {
        loadCommand.execute();
      }
      finally {
        String msg;
        if (loadComments != null && loadComments.length() > 0) {
          msg = "!" + Resources.getString("GameState.loaded", shortName) + ": <b>" + loadComments + "</b>"; //$NON-NLS-1$
        }
        else {
          msg = Resources.getString("GameState.loaded", shortName); //$NON-NLS-1$
        }
        GameModule.getGameModule().setGameFile(shortName, GameModule.GameFileMode.LOADED_GAME);
        GameModule.getGameModule().warn(msg);
      }
    }
  }


  public void loadGameInBackground(final File f) {
    try {
      loadGameInBackground(
        f.getName(),
        new BufferedInputStream(Files.newInputStream(f.toPath()))
      );
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, f);
    }
  }

  public void loadGameInBackground(final String shortName,
                                   final InputStream in)  {
    GameModule.getGameModule().warn(
      Resources.getString("GameState.loading", shortName));  //$NON-NLS-1$

    final JFrame frame = GameModule.getGameModule().getPlayerWindow();
    frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

    new SwingWorker<Command, Void>() {
      @Override
      public Command doInBackground() throws Exception {
        try (in) {
          return decodeSavedGame(in);
        }
      }

      @Override
      protected void done() {
        try {
          Command loadCommand = null;
          String msg = null;
          try {
            loadCommand = get();

            if (loadCommand != null) {
              if (loadComments != null && loadComments.length() > 0) {
                msg = "!" + Resources.getString("GameState.loaded", shortName) + ": <b>" + loadComments + "</b>"; //$NON-NLS-1$
              }
              else {
                msg = Resources.getString("GameState.loaded", shortName); //$NON-NLS-1$
              }
              GameModule.getGameModule().setGameFile(shortName, GameModule.GameFileMode.LOADED_GAME);
            }
            else {
              msg = Resources.getString("GameState.invalid_savefile", shortName);  //$NON-NLS-1$
              GameModule.getGameModule().setGameFileMode(GameModule.GameFileMode.NEW_GAME);
            }
          }
          catch (InterruptedException e) {
            ErrorDialog.bug(e);
          }
          // FIXME: review error message
          catch (ExecutionException e) {
// FIXME: This is a temporary hack to catch OutOfMemoryErrors; there should
// be a better, more uniform and more permanent way of handling these, since
// an OOME is neither a VASSAL bug, a module bug, nor due to bad data.
            final OutOfMemoryError oom =
              ThrowableUtils.getAncestor(OutOfMemoryError.class, e);
            if (oom != null) {
              ErrorDialog.bug(e);
            }
            else {
              log.error("", e);
            }
            msg = Resources.getString("GameState.error_loading", shortName);
          }

          if (loadCommand != null) {
            loadCommand.execute();
          }

          GameModule.getGameModule().warn(msg);
        }
        finally {
          frame.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
      }
    }.execute();
  }

  /**
   * @return a Command that, when executed, will add all pieces currently
   * in the game. Used when saving a game. Pieces are grouped by map, and
   * within a map by visual layer.
   */
  public Command getRestorePiecesCommand() {
    // TODO remove stacks that were empty when the game was loaded and are still empty now
    final List<GamePiece> pieceList = new ArrayList<>(pieces.values());
    pieceList.sort(new Comparator<>() {
      private final Map<GamePiece, Integer> indices = new HashMap<>();

      // Cache indices because indexOf() is linear;
      // otherwise sorting would be quadratic.
      private int indexOf(GamePiece p, VASSAL.build.module.Map m) {
        Integer pi = indices.get(p);
        if (pi == null) {
          indices.put(p, pi = m.getPieceCollection().indexOf(p));
        }
        return pi;
      }

      @Override
      public int compare(GamePiece a, GamePiece b) {
        final VASSAL.build.module.Map amap = a.getMap(), bmap = b.getMap();

        if (amap == null) {
          return bmap == null ?
            // order by id if neither piece is on a map
            a.getId().compareTo(b.getId()) :
            // nonnull map sorts before null map
            -1;
        }
        else if (bmap == null) {
          // null map sorts after nonnull map
          return 1;
        }
        else if (amap == bmap) {
          // same map, sort according to piece list
          return indexOf(a, amap) - indexOf(b, bmap);
        }
        else {
          // different maps, order by map
          return amap.getId().compareTo(bmap.getId());
        }
      }
    });

    final Command c = new NullCommand();
    for (final GamePiece p : pieceList) {
      c.append(new AddPiece(p));
    }
    return c;
  }

  /**
   * Read a saved game and translate it into a Command.  Executing the
   * command will load the saved game.
   *
   * @param saveFile Save file name
   * @return Command
   * @throws IOException I/O Exception
   */
  public Command decodeSavedGame(File saveFile) throws IOException {
    return decodeSavedGame(
      new BufferedInputStream(Files.newInputStream(saveFile.toPath())));
  }

  public Command decodeSavedGame(InputStream in) throws IOException {
    try (ZipInputStream zipInput = new ZipInputStream(in)) {
      for (ZipEntry entry = zipInput.getNextEntry(); entry != null;
           entry = zipInput.getNextEntry()) {
        if (SAVEFILE_ZIP_ENTRY.equals(entry.getName())) {
          try (InputStream din = new DeobfuscatingInputStream(zipInput)) {
            // FIXME: toString() is very inefficient, make decode() use the stream directly
            return GameModule.getGameModule().decode(
              IOUtils.toString(din, StandardCharsets.UTF_8)
            );
          }
        }
      }
    }

// FIXME: give more specific error message
    throw new IOException("Invalid saveFile format"); //NON-NLS
  }

  public DirectoryConfigurer getSavedGameDirectoryPreference() {
    if (savedGameDirectoryPreference == null) {
      savedGameDirectoryPreference = new DirectoryConfigurer("savedGameDir", null); //NON-NLS
      GameModule.getGameModule().getPrefs().addOption(null, savedGameDirectoryPreference);
    }
    return savedGameDirectoryPreference;
  }
}
