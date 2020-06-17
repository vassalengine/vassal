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

package VASSAL.build.module.map;

import VASSAL.counters.GamePiece;

/**
 * Manages a collection of {@link VASSAL.counters.GamePiece}s in a {@link VASSAL.build.module.Map}
 */
public interface PieceCollection {
  /** Reposition a piece to the front of all others in the same layer*/
  void moveToFront(GamePiece p);
  /** Reposition a piece to the back of all others in the same layer*/
  void moveToBack(GamePiece p);
  /** Return all currently-visible pieces in the collection as a read-only array */
  GamePiece[] getPieces();
  /** Return all pieces in the collection, regardless of visibility */
  GamePiece[] getAllPieces();
  /** Return true if the two pieces can be merged into a single stack */
  boolean canMerge(GamePiece p1, GamePiece p2);
  /**
   * Returns the index of a piece.  When painting the map, pieces
   * are drawn in order of index */
  int indexOf(GamePiece p);
  /** Removes the piece */
  void remove(GamePiece p);
  /** Adds a piece */
  void add(GamePiece p);
  /** Remove all pieces */
  void clear();
}
