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
import java.io.File;
import java.io.IOException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.counters.GamePiece;

/**
 * The GameState represents the state of the game currently being played.  Only one game can be open at once.
 * @see GameModule#getGameState */
public abstract class GameState {
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
    JFileChooser fc = GameModule.getGameModule().getFileChooser();
    if (fc.showOpenDialog(null) != JFileChooser.APPROVE_OPTION) return;

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

  /**
   * Return a {@link Command} that, when executed, will restore the
   * game to its current state.  Invokes {@link GameComponent#getRestoreCommand}
   * on each registered {@link GameComponent} */
  public abstract Command getRestoreCommand();

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
    File outputFile = null;
    JFileChooser fc = GameModule.getGameModule().getFileChooser();
    File sf = fc.getSelectedFile();
    if (sf != null) {
      String name = sf.getPath();
      if (name != null) {
        int index = name.lastIndexOf('.');
        if (index > 0) {
          name = name.substring(0, index) + ".sav";
          fc.setSelectedFile(new File(name));
        }
      }
    }

    if (fc.showSaveDialog(null) != JFileChooser.APPROVE_OPTION) return null;

    outputFile = fc.getSelectedFile();
    if (outputFile.exists() &&
        shouldConfirmOverwrite() &&
        JOptionPane.NO_OPTION ==
         JOptionPane.showConfirmDialog(GameModule.getGameModule().getFrame(),
          "Overwrite " + outputFile.getName() + "?", "File Exists",
          JOptionPane.YES_NO_OPTION)) {
        outputFile = null;
    } 

    return outputFile;
  }

  private boolean shouldConfirmOverwrite() {
    return System.getProperty("os.name").trim().equalsIgnoreCase("linux");
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

  public abstract void loadGame(File f) throws IOException;

  public abstract void saveGame(File f) throws IOException;

  public abstract void loadContinuation(File f) throws IOException;


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
}
