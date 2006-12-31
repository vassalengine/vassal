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
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import VASSAL.build.GameModule;
import VASSAL.command.AddPiece;
import VASSAL.command.AlertCommand;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.CommandFilter;
import VASSAL.command.ConditionalCommand;
import VASSAL.command.NullCommand;
import VASSAL.counters.GamePiece;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.BackgroundTask;
import VASSAL.tools.DataArchive;
import VASSAL.tools.Deobfuscator;
import VASSAL.tools.FileChooser;
import VASSAL.tools.Obfuscator;

/**
 * The GameState represents the state of the game currently being played.  Only one game can be open at once.
 * @see GameModule#getGameState */
public class GameState implements CommandEncoder {
  protected Hashtable pieces = new Hashtable();
  protected Vector gameComponents = new Vector();
  protected JMenuItem loadGame, saveGame, newGame, closeGame;
  protected String lastSave;

  public GameState() {
  }

  /**
   * Expects to be added to a GameModule.  Adds <code>New</code>,
   * <code>Load</code>, <code>Close</code>, and <code>Save</code>
   * entries to the <code>File</code> menu of the controls window */
  public void addTo(GameModule mod) {
    loadGame = new JMenuItem("Load Game");
    loadGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        loadGame();
      }
    });
    loadGame.setMnemonic('L');

    saveGame = new JMenuItem("Save Game");
    saveGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        saveGame();
      }
    });
    saveGame.setMnemonic('S');

    newGame = new JMenuItem("New Game");
    newGame.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setup(false);
        setup(true);
      }
    });
    newGame.setMnemonic('N');

    closeGame = new JMenuItem("Close Game");
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
    setup(false);
  }

  /**
   * @return true if the game state is different from when it was
   * last saved     */
  public boolean isModified() {
    String s = saveString();
    return s != null && !s.equals(lastSave);
  }

  /**
   * Add a {@link GameComponent} to the list of objects that will
   * be notified when a game is started/ended     */
  public void addGameComponent(GameComponent theComponent) {
    gameComponents.addElement(theComponent);
  }

  /**
   * Remove a {@link GameComponent} from the list of objects that will
   * be notified when a game is started/ended     */
  public void removeGameComponent(GameComponent theComponent) {
    gameComponents.removeElement(theComponent);
  }

  /**
   * @return an enumeration of all {@link GameComponent} objects
   * that have been added to this GameState */
  public Enumeration getGameComponentsEnum() {
    return gameComponents.elements();
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
          (GameModule.getGameModule().getFrame(), "Save Game?", "Game modified", JOptionPane.YES_NO_CANCEL_OPTION)) {
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
      loadGame.setText("Load Continuation");
    }
    else {
      loadGame.setText("Load Game");
      GameModule.getGameModule().appendToTitle(null);
    }

    gameStarted = gameStarted && this.gameStarting;
    for (Enumeration e = gameComponents.elements();
         e.hasMoreElements();) {
      GameComponent sub = (GameComponent) e.nextElement();
      sub.setup(this.gameStarting);
    }
    if (gameStarting) {
      GameModule.getGameModule().getDataArchive().clearScaledImageCache();
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
          loadGame(f);
        }
      }
      catch (IOException e) {
        String msg = "Unable to load " + f.getName();
        if (e.getMessage() != null) {
          msg += "\n" + e.getMessage();
        }
        JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(),
                               msg, "Load Error", JOptionPane.ERROR_MESSAGE);
      }
    }
    else {
      GameModule.getGameModule().warn("Unable to find " + f.getPath());
    }
  }

  protected String saveString() {
    return GameModule.getGameModule().encode(getRestoreCommand());
  }


  /** Prompts the user for a file into which to save the game */
  public void saveGame() {
    GameModule.getGameModule().warn("Saving game ...");
    try {
      File saveFile = getSaveFile();
      if (saveFile != null) {
        saveGame(saveFile);
        GameModule.getGameModule().warn("Game Saved");
      }
      else {
        GameModule.getGameModule().warn("Save Canceled");
      }
    }
    catch (IOException err) {
      GameModule.getGameModule().warn("Save Failed.  Try again.");
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
    return id == null ? null : (GamePiece) pieces.get(id);
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
    byte[] b = new Deobfuscator(getSaveFileInputStream(f.getPath())).getPlainText();
    Command c = GameModule.getGameModule().decode(new String(b, "UTF-8").trim());
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

  /**     * @return an Enumeration of all {@link GamePiece}s in the game.     */
  public Enumeration getPieces() {
    return pieces.elements();
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
  
  public static final String SAVEFILE_ZIP_ENTRY = "savedGame";

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
    for (Enumeration e = gameComponents.elements(); e.hasMoreElements();) {
      c.append(((GameComponent) e.nextElement()).getRestoreCommand());
    }
    c.append(new SetupCommand(true));
    return c;
  }

  private Command checkVersionCommand() {
    String runningVersion = GameModule.getGameModule().getAttributeValueString(GameModule.VASSAL_VERSION_RUNNING);
    ConditionalCommand.Condition cond = new ConditionalCommand.Lt(GameModule.VASSAL_VERSION_RUNNING, runningVersion);
    Command c = new ConditionalCommand(new ConditionalCommand.Condition[]{cond}, new AlertCommand("Version mismatch.\nGame saved using VASSAL version "
        + runningVersion + "."));
    String moduleName = GameModule.getGameModule().getAttributeValueString(GameModule.MODULE_NAME);
    String moduleVersion = GameModule.getGameModule().getAttributeValueString(GameModule.MODULE_VERSION);
    cond = new ConditionalCommand.Lt(GameModule.MODULE_VERSION, moduleVersion);
    c.append(new ConditionalCommand(new ConditionalCommand.Condition[]{cond}, new AlertCommand("Version mismatch.\nGame saved using " + moduleName
        + " version " + moduleVersion + ".")));
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
  public static final String BEGIN_SAVE = "begin_save";
  public static final String END_SAVE = "end_save";

  public void saveGame(File f) throws IOException {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    String save = saveString();
    new Obfuscator(save.getBytes("UTF-8")).write(out);
    out.close();
    lastSave = save;
    ArchiveWriter saver = new ArchiveWriter(f.getPath());
    saver.addFile(SAVEFILE_ZIP_ENTRY, new ByteArrayInputStream(out.toByteArray()));
    saver.write();
    if (saver.getArchive() != null) {
      saver.getArchive().close();
    }
  }

  public void loadGame(File f) throws IOException {
    final String name = f.getPath();
    final String shortName = f.getName();
    GameModule.getGameModule().warn("Loading " + shortName + " ...");
    new BackgroundTask() {
      private String msg;
      private Command loadCommand;

      public void doFirst() {
        try {
          byte b[] = new Deobfuscator(getSaveFileInputStream(name)).getPlainText();
          loadCommand = GameModule.getGameModule().decode(new String(b, "UTF-8").trim());
          if (loadCommand != null) {
            msg = "Loaded " + shortName;
          }
          else {
            msg = "Invalid savefile " + shortName;
          }
        }
        catch (Exception ex) {
          ex.printStackTrace();
          msg = "Error loading " + shortName;
        }
      }

      public void doLater() {
        if (loadCommand != null) {
          loadCommand.execute();
        }
        GameModule.getGameModule().warn(msg);
      }
    }.start();
  }

  private InputStream getSaveFileInputStream(final String name) throws IOException {
    InputStream in;
    try {
      in = DataArchive.getFileStream(new File(name), SAVEFILE_ZIP_ENTRY);
    }
    catch (IOException e) {
      in = new FileInputStream(name);
    }
    return in;
  }

  /**
   * @return a Command that, when executed, will add all pieces currently in the game. Used when saving a game.
   */
  public Command getRestorePiecesCommand() {
    List pieceList = new ArrayList();
    for (Enumeration e = pieces.elements(); e.hasMoreElements();) {
      GamePiece p = (GamePiece) e.nextElement();
      int index = 0;
      if (p.getParent() == null) {
        index = pieceList.size();
      }
      // TODO remove stacks that were empty when the game was loaded and are still empty now 
      pieceList.add(index, p);
    }
    Command c = new NullCommand();
    for (Iterator it = pieceList.iterator(); it.hasNext();) {
      GamePiece p = (GamePiece) it.next();
      c.append(new AddPiece(p));
    }
    return c;
  }

}
