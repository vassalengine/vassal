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

import VASSAL.command.Command;

/**
 * A GameComponent is any object which reacts to the start/end of a
 * game, or whose state can be saved as part of a game.  */

public interface GameComponent {
  /**
   * Notify the GameComponent that a game has started/ended
   * @param gameStarting if true, a game is starting.  If false, then a game is ending
   */
  public void setup(boolean gameStarting);


  /**
   * When saving a game, each GameComponent should return a {@link
   * Command} that, when executed, restores the GameComponent to its
   * state when the game was saved
   * If this component has no persistent state, return null
   */
  public Command getRestoreCommand();
}
