/*
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
  private final String command;
  private boolean isNewGame;

  public NewGameIndicator(String command) {
    this.command = command;
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(this);
  }

  @Override
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

  @Override
  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      isNewGame = true;
    }
  }

  @Override
  public Command decode(String command) {
    if (!command.startsWith(this.command)) {
      return null;
    }
    return new MarkGameNotNew(this);
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof MarkGameNotNew)
      || ((MarkGameNotNew) c).indicator != this) {
      return null;
    }

    return command;
  }

  public static class MarkGameNotNew extends Command {
    private final NewGameIndicator indicator;

    public MarkGameNotNew(NewGameIndicator indicator) {
      this.indicator = indicator;
    }

    @Override
    protected void executeCommand() {
      indicator.isNewGame = false;
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }
}
