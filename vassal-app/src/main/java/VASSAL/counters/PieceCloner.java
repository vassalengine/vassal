/*
 *
 * Copyright (c) 2004-2008 by Rodney Kinney, Joel Uckelman
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
import VASSAL.tools.ReflectionUtils;

/**
 * Utility class for cloning {@link GamePiece}s
 */
public class PieceCloner {
  private static final PieceCloner instance = new PieceCloner();

  // For use by subclasses
  protected PieceCloner() {

  }

  public static PieceCloner getInstance() {
    return instance;
  }

  /**
   * Create a new instance that is a clone of the given piece.
   *
   * @return the new instance
   */
  public GamePiece clonePiece(GamePiece piece) {
    GamePiece clone = null;
    if (piece instanceof BasicPiece) {
      clone = GameModule.getGameModule().createPiece(piece.getType());
      final Map m = piece.getMap();

      // Temporarily set map to null so that clone won't be added to map
      piece.setMap(null);

      clone.setState(piece.getState());
      piece.setMap(m);
    }
    else if (piece instanceof UsePrototype) {
      clone = clonePiece(((UsePrototype)piece).getExpandedInner());
    }
    else if (piece instanceof EditablePiece && piece instanceof Decorator) {
      try {
        clone = piece.getClass().getConstructor().newInstance();
        final Decorator dclone = (Decorator) clone;
        final Decorator dpiece = (Decorator) piece;

        dclone.setInner(clonePiece(dpiece.getInner()));
        ((EditablePiece)clone).mySetType(dpiece.myGetType());
        dclone.mySetState(dpiece.myGetState());
      }
      catch (Throwable t) {
        ReflectionUtils.handleNewInstanceFailure(t, piece.getClass());
      }
    }
    else {
      clone = ((AddPiece) GameModule.getGameModule().decode(
        GameModule.getGameModule().encode(new AddPiece(piece)))).getTarget();
      final Map m = piece.getMap();

      // Temporarily set map to null so that clone won't be added to map
      piece.setMap(null);

      clone.setState(piece.getState());
      piece.setMap(m);
    }
    return clone;
  }

  public GamePiece clonePieceUnexpanded(GamePiece piece) {
    GamePiece clone = null;
    if (piece instanceof BasicPiece) {
      clone = GameModule.getGameModule().createPiece(piece.getType());
      final Map m = piece.getMap();

      // Temporarily set map to null so that clone won't be added to map
      piece.setMap(null);

      clone.setState(piece.getState());
      piece.setMap(m);
    }
    else if (piece instanceof EditablePiece && piece instanceof Decorator) {
      try {
        clone = piece.getClass().getConstructor().newInstance();
        final Decorator dclone = (Decorator) clone;
        final Decorator dpiece = (Decorator) piece;

        dclone.setInner(clonePieceUnexpanded(dpiece.getInner()));
        ((EditablePiece)clone).mySetType(dpiece.myGetType());
        dclone.mySetState(dpiece.myGetState());
      }
      catch (Throwable t) {
        ReflectionUtils.handleNewInstanceFailure(t, piece.getClass());
      }
    }
    else {
      clone = ((AddPiece) GameModule.getGameModule().decode(
        GameModule.getGameModule().encode(new AddPiece(piece)))).getTarget();
      final Map m = piece.getMap();

      // Temporarily set map to null so that clone won't be added to map
      piece.setMap(null);

      clone.setState(piece.getState());
      piece.setMap(m);
    }
    return clone;
  }
}

