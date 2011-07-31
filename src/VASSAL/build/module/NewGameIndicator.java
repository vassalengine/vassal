/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;

/**
 * Provides information about whether a game was started from File->New Game
 * or loaded from a saved game
 */
public class NewGameIndicator implements GameComponent, CommandEncoder {
  private String command;
  private boolean isNewGame;

  public NewGameIndicator(String command) {
    this.command = command;
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(this);
  }

  public Command getRestoreCommand() {
    return new MarkGameNotNew(this);
  }

  /**
   *
   * @return true if the current game was started from the menu,
   * false if it was loaded from a saved game or logfile.
   */
  public boolean isNewGame() {
    return isNewGame;
  }

  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      isNewGame = true;
    }
  }

  public Command decode(String command) {
    Command c = null;
    if (command.startsWith(this.command)) {
      return new MarkGameNotNew(this);
    }
    return c;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof MarkGameNotNew
      && ((MarkGameNotNew)c).indicator == this) {
      s = command;
    }
    return s;
  }

  public static class MarkGameNotNew extends Command {
    private NewGameIndicator indicator;

    public MarkGameNotNew(NewGameIndicator indicator) {
      this.indicator = indicator;
    }

    protected void executeCommand() {
      indicator.isNewGame = false;
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
