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
package VASSAL.command;

import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

/**
 * This class makes it more convenient to create a ChangePiece command.
 * A TrackPiece is instantiated with a GamePiece and later finalized.
 * The old state is the state of the GamePIece at instantiation, while
 * the new state is the state of the GamePiece at finalization.
 * @deprecated Use {@link ChangeTracker}
 */
@Deprecated public class TrackPiece extends ChangePiece {
  private GamePiece piece;

  public TrackPiece(GamePiece p) {
    super(p.getId(), Decorator.getOutermost(p).getState(), null);
    piece = p;
  }

  public String getNewState() {
    if (newState == null) {
      throw new RuntimeException("Must invoke finalize() before getting new state");
    }
    return super.getNewState();
  }

  public void finalize() {
    newState = Decorator.getOutermost(piece).getState();
  }
}
