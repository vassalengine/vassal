/*
 * $Id$
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import VASSAL.build.GameModule;
import VASSAL.command.AddPiece;
import VASSAL.command.AlertCommand;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.CommandFilter;
import VASSAL.command.ConditionalCommand;
import VASSAL.command.Logger;
import VASSAL.command.NullCommand;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.BackgroundTask;
import VASSAL.tools.Deobfuscator;
import VASSAL.tools.FileChooser;
import VASSAL.tools.Obfuscator;

/**
 * The GameState represents the state of the game currently being played.
 * Only one game can be open at once.
 * @see GameModule#getGameState */
public class GameState implements CommandEncoder {
  protected Map<String,GamePiece> pieces = new HashMap<String,GamePiece>();
  protected List<GameComponent> gameComponents = new ArrayList<GameComponent>();
  protected List<GameSetupStep> setupSteps = new ArrayList<GameSetupStep>();
  protected JMenuItem loadGame, saveGame, newGame, closeGame;
  protected String lastSave;

  public GameState() {
  }

  /**
   * Expects to be added to a GameModule.  Adds <code>New</code>,
   * <code>Load</code>, <code>Close</code>, and <code>Save</code>
   * entries to the <code>File</code> menu of the controls window */
  public void addTo(GameModule mod) {
    loadGame = new JMenuItem(Resources.getString("GameState.load_game"));  //$NON-NLS-1$
    loadGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        loadGame();
      }
    });
    loadGame.setMnemonic('L');

    saveGame = new JMenuItem(Resources.getString("GameState.save_game"));  //$NON-NLS-1$
    saveGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        saveGame();
      }
    });
    saveGame.setMnemonic('S');

    newGame = new JMenuItem(Resources.getString("GameState.new_game"));  //$NON-NLS-1$
    newGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setup(false);
        setup(true);
      }
    });
    newGame.setMnemonic('N');

    closeGame = new JMenuItem(Resources.getString("GameState.close_game"));  //$NON-NLS-1$
    closeGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setup(false);
      }
    });
    closeGame.setMnemonic('C');

    mod.getFileMenu().insert(newGame, 0);
    mod.getFileMenu().add(loadGame);
    mod.getFileMenu().add(saveGame);
    mod.getFileMenu().add(closeGame);

    saveGame.setEnabled(gameStarting);
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
  public Enumeration getGameComponentsEnum() {
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
  public Iterator getUnfinishedSetupSteps() {
    ArrayList<GameSetupStep> l = new ArrayList<GameSetupStep>();
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

  /**
   * Start/end a game.  Prompt to save if the game state has been
   * modified since last save.  Invoke {@link GameComponent#setup}
   * on all registered {@link GameComponent} objects */
  public void setup(boolean gameStarting) {
    if (!gameStarting && gameStarted && isModified()) {
      switch (JOptionPane.showConfirmDialog
          (GameModule.getGameModule().getFrame(), Resources.getString("GameState.save_game_query"), Resources.getString("GameState.game_modified"), JOptionPane.YES_NO_CANCEL_OPTION)) {  //$NON-NLS-1$ //$NON-NLS-2$
        case JOptionPane.YES_OPTION:
          saveGame();
          break;
        case JOptionPane.CANCEL_OPTION:
          return;
      }
    }
    this.gameStarting = gameStarting;
    if (!gameStarting) {
      pieces.clear();
    }
    saveGame.setEnabled(gameStarting);
    closeGame.setEnabled(gameStarting);
    if (gameStarting) {
      loadGame.setText(Resources.getString("GameState.load_continuation"));  //$NON-NLS-1$
      GameModule.getGameModule().getWizardSupport().showGameSetupWizard();
    }
    else {
      loadGame.setText(Resources.getString("GameState.load_game"));  //$NON-NLS-1$
      GameModule.getGameModule().appendToTitle(null);
    }

    gameStarted = gameStarted && this.gameStarting;
    for (GameComponent gc : gameComponents) {
      gc.setup(this.gameStarting);
    }
    if (gameStarting) {
      GameModule.getGameModule().getDataArchive().clearTransformedImageCache();
    }
    gameStarted = gameStarted || this.gameStarting;
    lastSave = gameStarting ? saveString() : null;
  }

  public boolean isGameStarted() {
    return gameStarted;
  }

  /**
   * Read the game from a savefile.  The contents of the file is
   * sent to {@link GameModule#decode} and translated into a
   * {@link Command}, which is then executed.  The command read from the
   * file should be that returned by {@link #getRestoreCommand} */
  public void loadGame() {
    FileChooser fc = GameModule.getGameModule().getFileChooser();
    if (fc.showOpenDialog() != FileChooser.APPROVE_OPTION) return;

    File f = fc.getSelectedFile();
    if (f.exists()) {
      try {
        if (gameStarted) {
          loadContinuation(f);
        }
        else {
          loadGameInBackground(f);
        }
      }
      catch (IOException e) {
        String msg = Resources.getString("GameState.unable_to_load", f.getName());  //$NON-NLS-1$
        if (e.getMessage() != null) {
          msg += "\n" + e.getMessage();  //$NON-NLS-1$
        }
        JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(),
                               msg, Resources.getString("GameState.load_error"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$
      }
    }
    else {
      GameModule.getGameModule().warn(Resources.getString("GameState.unable_to_find", f.getPath()));  //$NON-NLS-1$
    }
  }

  protected String saveString() {
    return GameModule.getGameModule().encode(getRestoreCommand());
  }


  /** Prompts the user for a file into which to save the game */
  public void saveGame() {
    GameModule.getGameModule().warn(Resources.getString("GameState.saving_game"));  //$NON-NLS-1$
    try {
      File saveFile = getSaveFile();
      if (saveFile != null) {
        saveGame(saveFile);
        GameModule.getGameModule().warn(Resources.getString("GameState.game_saved"));  //$NON-NLS-1$
      }
      else {
        GameModule.getGameModule().warn(Resources.getString("GameState.save_canceled"));  //$NON-NLS-1$
      }
    }
    catch (IOException err) {
      GameModule.getGameModule().warn(Resources.getString("GameState.save_failed"));  //$NON-NLS-1$
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
    FileChooser fc = GameModule.getGameModule().getFileChooser();
    fc.selectDotSavFile();
    if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) return null;

    File outputFile = fc.getSelectedFile();
    return outputFile;
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
    String id = (new Long(time)).toString();
    while (pieces.get(id) != null) {
      time++;
      id = (new Long(time)).toString();
    }
    return id;
  }

  public void loadContinuation(File f) throws IOException {
    Command c = decodeSavedGame(f);
    CommandFilter filter = new CommandFilter() {
      protected boolean accept(Command c) {
        return c instanceof BasicLogger.LogCommand;
      }
    };
    c = filter.apply(c);
    if (c != null) {
      c.execute();
    }
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

    protected void executeCommand() {
      GameModule.getGameModule().getGameState().setup(gameStarting);
    }

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
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    String save = saveString();
    new Obfuscator(save.getBytes("UTF-8")).write(out);  //$NON-NLS-1$
    out.close();
    lastSave = save;
    ArchiveWriter saver = new ArchiveWriter(f.getPath());
    saver.addFile(SAVEFILE_ZIP_ENTRY, new ByteArrayInputStream(out.toByteArray()));
    saver.write();
    if (saver.getArchive() != null) {
      saver.getArchive().close();
    }
  }

  public void loadGameInBackground(final File f) {
    try {
      loadGameInBackground(f.getName(), new FileInputStream(f));
    }
    catch (FileNotFoundException e) {
      GameModule.getGameModule().warn(Resources.getString("GameState.invalid_savefile", f.getPath()));
    }
  }

  public void loadGameInBackground(final String shortName, final InputStream in)  {
    GameModule.getGameModule().warn(Resources.getString("GameState.loading", shortName));  //$NON-NLS-1$
    new BackgroundTask() {
      private String msg;
      private Command loadCommand;

      public void doFirst() {
        try {
          loadCommand = decodeSavedGame(in);
          if (loadCommand != null) {
            msg = Resources.getString("GameState.loaded", shortName);  //$NON-NLS-1$
          }
          else {
            msg = Resources.getString("GameState.invalid_savefile", shortName);  //$NON-NLS-1$
          }
        }
        catch (Exception ex) {
          ex.printStackTrace();
          msg = Resources.getString("GameState.error_loading", shortName);  //$NON-NLS-1$
        }
      }

      public void doLater() {
        if (loadCommand != null) {
          loadCommand.execute();
        }
        GameModule.getGameModule().warn(msg);
        Logger logger = GameModule.getGameModule().getLogger();
        if (logger instanceof BasicLogger) {
          ((BasicLogger)logger).queryNewLogFile(true);
        }
      }
    }.start();
  }

  /**
   * @return a Command that, when executed, will add all pieces currently
   * in the game. Used when saving a game.
   */
  public Command getRestorePiecesCommand() {
    ArrayList<GamePiece> pieceList = new ArrayList<GamePiece>();
    for (GamePiece p : pieces.values()) {
      int index = 0;
      if (p.getParent() == null) {
        index = pieceList.size();
      }
      // TODO remove stacks that were empty when the game was loaded and are still empty now 
      pieceList.add(index, p);
    }
    Command c = new NullCommand();
    for (GamePiece p : pieceList) {
      c.append(new AddPiece(p));
    }
    return c;
  }

  /**
   * Read a saved game and translate it into a Command.  Executing the command will load the saved game 
   * @param fileName
   * @return
   * @throws IOException
   */
  public Command decodeSavedGame(File saveFile) throws IOException {
    return decodeSavedGame(new FileInputStream(saveFile));
  }
  
  public Command decodeSavedGame(InputStream in) throws IOException {
    ZipInputStream zipInput = new ZipInputStream(in);
    for (ZipEntry entry = zipInput.getNextEntry(); entry != null; entry = zipInput.getNextEntry()) {
      if (SAVEFILE_ZIP_ENTRY.equals(entry.getName())) {
        byte b[] = new Deobfuscator(zipInput).getPlainText();
        return GameModule.getGameModule().decode(new String(b, "UTF-8").trim());
      }
    }
    throw new IOException("Invalid saveFile format");
  }
}
