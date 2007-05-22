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

package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.command.AddPiece;

/**
 * Utility class for cloning {@link GamePiece}s
 */
public class PieceCloner {
  private static PieceCloner instance;

  // For use by sub-classes
  protected PieceCloner() {
  }

  public static PieceCloner getInstance() {
    if (instance == null) {
      instance = new PieceCloner();
    }
    return instance;
  }
  /**
   * Create a new instance that is a clone of the given piece
   * @return the new instance
   */
  public GamePiece clonePiece(GamePiece piece) {
    GamePiece clone = null;
    if (piece instanceof BasicPiece) {
      clone = GameModule.getGameModule().createPiece(piece.getType());
      Map m = piece.getMap();
      piece.setMap(null); // Temporarily set map to null so that clone won't be added to map
      clone.setState(piece.getState());
      piece.setMap(m);
    }
    else if (piece instanceof UsePrototype) {
      clone = clonePiece(((UsePrototype)piece).getExpandedInner());
    }
    else if (piece instanceof EditablePiece
        && piece instanceof Decorator) {
      try {
        clone = piece.getClass().newInstance();
        ((Decorator) clone).setInner(clonePiece(((Decorator) piece).getInner()));
        ((EditablePiece) clone).mySetType(((Decorator) piece).myGetType());
        ((Decorator) clone).mySetState(((Decorator) piece).myGetState());
      }
      catch (InstantiationException e) {
        throw new RuntimeException(e);
      }
      catch (IllegalAccessException e) {
        throw new RuntimeException(e);
      }
    }
    else {
      clone = ((AddPiece) GameModule.getGameModule().decode
          (GameModule.getGameModule().encode
           (new AddPiece(piece)))).getTarget();
      Map m = piece.getMap();
      piece.setMap(null); // Temporarily set map to null so that clone won't be added to map
      clone.setState(piece.getState());
      piece.setMap(m);
    }
    return clone;
  }
}
