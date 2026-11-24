/*
 *
 * Copyright (c) 2003-2011 by Rodney Kinney, Brent Easton
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
package VASSAL.command;

import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

/**
 * Utility class for generating {@link ChangePiece} commands.
 * To use, construct a new ChangeTracker, make any changes
 * to the target {@link VASSAL.counters.GamePiece}, then invoke {@link #getChangeCommand}
 */
public class ChangeTracker {
  private final GamePiece piece;
  private final String oldState;

  public ChangeTracker(GamePiece p) {
    oldState = Decorator.getOutermost(p).getState();
    piece = p;
  }

  public Command getChangeCommand() {
    return new ChangePiece(piece.getId(), oldState, Decorator.getOutermost(piece).getState());
  }

  public boolean isChanged() {
    return !oldState.equals(Decorator.getOutermost(piece).getState());
  }

  /**
   * Like {@link #getChangeCommand}, but returns null if the piece's state has not changed.
   * <p>
   * The new state is taken only once, so this costs one full-piece serialisation where
   * {@code isChanged() ? getChangeCommand() : null} costs two, and it never produces a
   * no-op {@link ChangePiece} to be logged and sent to the other players.
   *
   * @return a {@link ChangePiece} command, or null if the state is unchanged
   */
  public Command getChangeCommandIfChanged() {
    final String newState = Decorator.getOutermost(piece).getState();
    return oldState.equals(newState) ? null : new ChangePiece(piece.getId(), oldState, newState);
  }
}
