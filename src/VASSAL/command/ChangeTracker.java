package VASSAL.command;

import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

/*
 * $Id$
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

/**
 * Utility class for generating {@link ChangePiece} commands.
 * To use, construct a new ChangeTracker, make any changes
 * to the target {@link VASSAL.counters.GamePiece}, then invoke {@link #getChangeCommand}
 */
public class ChangeTracker {
  private GamePiece piece;
  private String oldState;

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
}
