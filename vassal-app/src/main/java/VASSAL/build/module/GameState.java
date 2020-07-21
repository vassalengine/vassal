/*
 *
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

import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
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
import VASSAL.launch.Launcher;
import VASSAL.tools.ComponentSplitter;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.WarningDialog;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.LogAndSaveFileFilter;
import VASSAL.tools.io.DeobfuscatingInputStream;
import VASSAL.tools.io.FastByteArrayOutputStream;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.ObfuscatingOutputStream;
import VASSAL.tools.io.ZipArchive;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.Dialogs;


/**
 * The GameState represents the state of the game currently being played.
 * Only one game can be open at once.
 * @see GameModule#getGameState
 */
public class GameState implements CommandEncoder {
  private static final org.slf4j.Logger log =
    LoggerFactory.getLogger(GameState.class);

  protected Map<String,GamePiece> pieces = new HashMap<>();
  protected List<GameComponent> gameComponents = new ArrayList<>();
  protected List<GameSetupStep> setupSteps = new ArrayList<>();
  protected Action loadGame, saveGame, saveGameAs, newGame, closeGame;
  protected String lastSave;
  protected File lastSaveFile = null;
  protected DirectoryConfigurer savedGameDirectoryPreference;
  protected String loadComments;

  public GameState() {}

  /**
   * Expects to be added to a GameModule.  Adds <code>New</code>,
   * <code>Load</code>, <code>Close</code>, and <code>Save</code>
   * entries to the <code>File</code> menu of the controls window
   */
  public void addTo(GameModule mod) {
    loadGame = new AbstractAction(Resources.getString("GameState.load_game")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        loadGame();
      }
    };
    // FIMXE: setting nmemonic from first letter could cause collisions in
    // some languages
    loadGame.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.load_game.shortcut").charAt(0));

    saveGame = new AbstractAction(Resources.getString("GameState.save_game")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        saveGame();
      }
    };
    // FIMXE: setting nmemonic from first letter could cause collisions in
    // some languages
    saveGame.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.save_game.shortcut").charAt(0));

    saveGameAs = new AbstractAction(Resources.getString("GameState.save_game_as")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        saveGameAs();
      }
    };
    // FIMXE: setting nmemonic from first letter could cause collisions in
    // some languages
    saveGameAs.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.save_game_as.shortcut").charAt(0));

    newGame = new AbstractAction(Resources.getString("GameState.new_game")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        setup(false);
        setup(true);
      }
    };
    // FIMXE: setting nmemonic from first letter could cause collisions in
    // some languages
    newGame.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.new_game.shortcut").charAt(0));

    closeGame = new AbstractAction(
        Resources.getString("GameState.close_game")) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        setup(false);
      }
    };
    // FIMXE: setting nmemonic from first letter could cause collisions in
    // some languages
    closeGame.putValue(Action.MNEMONIC_KEY, (int)Resources.getString("GameState.close_game.shortcut").charAt(0));

    final MenuManager mm = MenuManager.getInstance();
    mm.addAction("GameState.new_game", newGame);
    mm.addAction("GameState.load_game", loadGame);
    mm.addAction("GameState.save_game", saveGame);
    mm.addAction("GameState.save_game_as", saveGameAs);
    mm.addAction("GameState.close_game", closeGame);

    saveGame.setEnabled(gameStarting);
    saveGameAs.setEnabled(gameStarting);
    closeGame.setEnabled(gameStarting);
  }

  /**
   * @return true if the game state is different from when it was last saved
   */
  public boolean isModified() {
    String s = saveString();
    return s != null && !s.equals(lastSave);
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
  @Deprecated
  public Enumeration<GameComponent> getGameComponentsEnum() {
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
    ArrayList<GameSetupStep> l = new ArrayList<>();
    for (GameSetupStep step : setupSteps) {
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

  /**
   * Start a game for updating (via editor).
   * <em>NOTE: This method is not for use in custom code.</em>
   */
  public void setup(boolean gameStarting, boolean gameUpdating) {
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

    if (!gameStarting && gameStarted && isModified()) {
      switch (JOptionPane.showConfirmDialog(
        g.getFrame(),
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

    if (gameStarting) {
      loadGame.putValue(Action.NAME,
        Resources.getString("GameState.load_continuation"));
      g.getWizardSupport().showGameSetupWizard();
    }
    else {
      loadGame.putValue(Action.NAME,
        Resources.getString("GameState.load_game"));
      g.appendToTitle(null);
    }

    gameStarted &= this.gameStarting;
    for (GameComponent gc : gameComponents) {
      gc.setup(this.gameStarting);
    }

    gameStarted |= this.gameStarting;
    lastSave = gameStarting ? saveString() : null;
    lastSaveFile = null;

    if (gameStarted) {
      adjustSplitter();

      if (gameStarting) {
        SwingUtilities.invokeLater(new Runnable() {
          @Override
          public void run() {
            Logger logger = GameModule.getGameModule().getLogger();
            if (logger instanceof BasicLogger) {
              ((BasicLogger)logger).queryNewLogFile(true);
            }
          }
        });
      }
    }
  }

  private void adjustSplitter() {
    // If there is a docked map, set the splitter to a reasaonable location
    final GameModule g = GameModule.getGameModule();
    for (VASSAL.build.module.Map m : g.getComponentsOf(VASSAL.build.module.Map.class)) {
      if (m.shouldDockIntoMainWindow()) {
        final Component c = SwingUtilities.getAncestorOfClass(
          ComponentSplitter.SplitPane.class, m.getView()
        );

        // this should always be true
        if (c instanceof ComponentSplitter.SplitPane) {
          final ComponentSplitter.SplitPane sp = (ComponentSplitter.SplitPane) c;

          SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
              sp.setDividerLocation(g.getChatter().getPreferredSize().height);
            }
          });
        }
        return;
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
   */
  public void loadGame() {
    final GameModule g = GameModule.getGameModule();

    loadComments = "";
    final FileChooser fc = g.getFileChooser();
    fc.addChoosableFileFilter(new LogAndSaveFileFilter());

    if (fc.showOpenDialog() != FileChooser.APPROVE_OPTION) return;

    final File f = fc.getSelectedFile();
    try {
      if (!f.exists()) throw new FileNotFoundException(
        "Unable to locate " + f.getPath());

      // Check the Save game for validity
      final AbstractMetaData metaData = MetaDataFactory.buildMetaData(f);
      if (metaData == null || ! (metaData instanceof SaveMetaData)) {
        WarningDialog.show("GameState.invalid_save_file", f.getPath());
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
        "Loading save game " + f.getPath() +
        ", created with module version " + saveModuleVersion
      );

      if (gameStarted) {
        loadContinuation(f);
      }
      else {
        loadGameInBackground(f);
      }

      lastSaveFile = f;
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, f);
    }
/*
        String msg = Resources.getString("GameState.unable_to_load", f.getName());  //$NON-NLS-1$
        if (e.getMessage() != null) {
          msg += "\n" + e.getMessage();  //$NON-NLS-1$
        }
        JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(),
                               msg, Resources.getString("GameState.load_error"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$
    }
    else {
// FIXME: give more specific error message
// FIXME: maybe deprecate warn()?
      GameModule.getGameModule().warn(Resources.getString("GameState.unable_to_find", f.getPath()));  //$NON-NLS-1$
    }
*/
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
            GameModule.getGameModule().getFrame(),
            Resources.getString("Warning.save_will_be_updated_title"),
            Resources.getString("Warning.save_will_be_updated_heading"),
            Resources.getString(
              "Warning.save_will_be_updated_message",
              f.getPath(),
              "3.2"
            ),
              JOptionPane.WARNING_MESSAGE,
            JOptionPane.OK_CANCEL_OPTION) != JOptionPane.CANCEL_OPTION;
        }
      }
    }

    return true;
  }

  /** Saves the game to an existing file, or prompts for a new one. */
  public void saveGame() {
    final GameModule g = GameModule.getGameModule();

    g.warn(Resources.getString("GameState.saving_game"));  //$NON-NLS-1$

    if (lastSaveFile != null) {
      if (!checkForOldSaveFile(lastSaveFile)) {
        return;
      }

      try {
        saveGame(lastSaveFile);
        g.warn(Resources.getString("GameState.game_saved"));  //$NON-NLS-1$
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, lastSaveFile);
/*
        Logger.log(err);
        GameModule.getGameModule().warn(Resources.getString("GameState.save_failed"));  //$NON-NLS-1$
*/
      }
    }
    else {
      saveGameAs();
    }
  }

  /** Prompts the user for a file into which to save the game */
  public void saveGameAs() {
    final GameModule g = GameModule.getGameModule();

    g.warn(Resources.getString("GameState.saving_game"));  //$NON-NLS-1$

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
        g.warn(Resources.getString("GameState.game_saved"));  //$NON-NLS-1$
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, saveFile);
/*
        Logger.log(err);
        GameModule.getGameModule().warn(Resources.getString("GameState.save_failed"));  //$NON-NLS-1$
*/
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
    if (file.getName().indexOf('.') == -1)
      file = new File(file.getParent(), file.getName() + ".vsav");

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
    GameModule.getGameModule().warn(
        Resources.getString("GameState.loading", f.getName()));  //$NON-NLS-1$
    Command c = decodeSavedGame(f);
    CommandFilter filter = new CommandFilter() {
      @Override
      protected boolean accept(Command c) {
        return c instanceof BasicLogger.LogCommand;
      }
    };
    c = filter.apply(c);
    if (c != null) {
      c.execute();
    }
    String msg = Resources.getString("GameState.loaded", f.getName());  //$NON-NLS-1$
    if (loadComments != null && loadComments.length() > 0) {
      msg += ": " + loadComments;
    }
    GameModule.getGameModule().warn(msg);
  }

  /**
   * @return an Enumeration of all {@link GamePiece}s in the game
   * @deprecated Use {@link #getAllPieces()} instead.
   */
  @Deprecated
  public Enumeration<GamePiece> getPieces() {
    return Collections.enumeration(pieces.values());
  }

  /** @return a Collection of all {@link GamePiece}s in the game */
  public Collection<GamePiece> getAllPieces() {
    return pieces.values();
  }

  public static class SetupCommand extends Command {
    private boolean gameStarting;

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
   * Return a {@link Command} that, when executed, will restore the
   * game to its current state.  Invokes {@link GameComponent#getRestoreCommand}
   * on each registered {@link GameComponent} */
  public Command getRestoreCommand() {
    if (!saveGame.isEnabled()) {
      return null;
    }
    Command c = new SetupCommand(false);
    c.append(checkVersionCommand());
    c.append(getRestorePiecesCommand());
    for (GameComponent gc : gameComponents) {
      c.append(gc.getRestoreCommand());
    }
    c.append(new SetupCommand(true));
    return c;
  }

  private Command checkVersionCommand() {
    String runningVersion = GameModule.getGameModule().getAttributeValueString(GameModule.VASSAL_VERSION_RUNNING);
    ConditionalCommand.Condition cond = new ConditionalCommand.Lt(GameModule.VASSAL_VERSION_RUNNING, runningVersion);
    Command c = new ConditionalCommand(new ConditionalCommand.Condition[]{cond}, new AlertCommand(Resources.getString("GameState.version_mismatch", runningVersion)));  //$NON-NLS-1$
    String moduleName = GameModule.getGameModule().getAttributeValueString(GameModule.MODULE_NAME);
    String moduleVersion = GameModule.getGameModule().getAttributeValueString(GameModule.MODULE_VERSION);
    cond = new ConditionalCommand.Lt(GameModule.MODULE_VERSION, moduleVersion);
    c.append(new ConditionalCommand(new ConditionalCommand.Condition[]{cond}, new AlertCommand(Resources.getString("GameState.version_mismatch2", moduleName, moduleVersion ))));  //$NON-NLS-1$
    return c;
  }

  /**
   * A GameState recognizes instances of {@link SetupCommand}
   */
  @Override
  public String encode(Command c) {
    if (c instanceof SetupCommand) {
      return ((SetupCommand) c).isGameStarting() ? END_SAVE : BEGIN_SAVE;
    }
    else {
      return null;
    }
  }

  /**
   * A GameState recognizes instances of {@link SetupCommand}
   */
  @Override
  public Command decode(String theCommand) {
    if (BEGIN_SAVE.equals(theCommand)) {
      return new SetupCommand(false);
    }
    else if (END_SAVE.equals(theCommand)) {
      return new SetupCommand(true);
    }
    else {
      return null;
    }
  }
  public static final String BEGIN_SAVE = "begin_save";  //$NON-NLS-1$
  public static final String END_SAVE = "end_save";  //$NON-NLS-1$

  public void saveGame(File f) throws IOException {
// FIXME: Extremely inefficient! Write directly to ZipArchive OutputStream
    final String save = saveString();
    final FastByteArrayOutputStream ba = new FastByteArrayOutputStream();
    try (OutputStream out = new ObfuscatingOutputStream(ba)) {
      out.write(save.getBytes(StandardCharsets.UTF_8));
    }

    try (FileArchive archive = new ZipArchive(f)) {
      archive.add(SAVEFILE_ZIP_ENTRY, ba.toInputStream());
      (new SaveMetaData()).save(archive);
    }

    Launcher.getInstance().sendSaveCmd(f);

    setModified(false);
  }

  public void loadGameInBackground(final File f) {
    try {
      loadGameInBackground(f.getName(),
                           new BufferedInputStream(new FileInputStream(f)));
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, f);
    }
  }

  public void loadGameInBackground(final String shortName,
                                   final InputStream in)  {
    GameModule.getGameModule().warn(
      Resources.getString("GameState.loading", shortName));  //$NON-NLS-1$

    final JFrame frame = GameModule.getGameModule().getFrame();
    frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

    new SwingWorker<Command,Void>() {
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
              msg = Resources.getString("GameState.loaded", shortName);  //$NON-NLS-1$
              if (loadComments != null && loadComments.length() > 0) {
                msg += ": " + loadComments;
              }
            }
            else {
              msg = Resources.getString("GameState.invalid_savefile", shortName);  //$NON-NLS-1$
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
   * in the game. Used when saving a game.
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
    for (GamePiece p : pieceList) {
      c.append(new AddPiece(p));
    }
    return c;
  }

  /**
   * Read a saved game and translate it into a Command.  Executing the
   * command will load the saved game.
   *
   * @param fileName
   * @return
   * @throws IOException
   */
  public Command decodeSavedGame(File saveFile) throws IOException {
    return decodeSavedGame(
      new BufferedInputStream(new FileInputStream(saveFile)));
  }

  public Command decodeSavedGame(InputStream in) throws IOException {
    try (ZipInputStream zipInput = new ZipInputStream(in)) {
      for (ZipEntry entry = zipInput.getNextEntry(); entry != null;
           entry = zipInput.getNextEntry()) {
        if (SAVEFILE_ZIP_ENTRY.equals(entry.getName())) {
          try (InputStream din = new DeobfuscatingInputStream(zipInput)) {
            // FIXME: toString() is very inefficient, make decode() use the stream directly
            final Command c = GameModule.getGameModule().decode(
              IOUtils.toString(din, StandardCharsets.UTF_8));
            return c;
          }
        }
      }
    }

// FIXME: give more specific error message
    throw new IOException("Invalid saveFile format");
  }

  public DirectoryConfigurer getSavedGameDirectoryPreference() {
    if (savedGameDirectoryPreference == null) {
      savedGameDirectoryPreference = new DirectoryConfigurer("savedGameDir", null);
      GameModule.getGameModule().getPrefs().addOption(null,savedGameDirectoryPreference);
    }
    return savedGameDirectoryPreference;
  }
}
