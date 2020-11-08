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

package VASSAL.build.module.map;

import VASSAL.counters.GamePiece;

/**
 * A PieceCollection generally holds a list of {@link VASSAL.counters.GamePiece}s on a {@link VASSAL.build.module.Map}.
 * Each Map has a master {@link CompoundPieceCollection} that contains all of its pieces, stacks, and decks -- usually
 * this will be a {@link LayeredPieceCollection} but possibly a simpler {@link DefaultPieceCollection}. Within a
 * CompoundPieceCollection, there may be several {@link SimplePieceCollection}s, each containing the pieces in a single
 * visual layer.
 * <br><br>
 * In addition to providing a means to maintain a list of all the pieces in a map, this interface provides a means to
 * manage the draw order of pieces, in part by grouping pieces into different visual layers for e.g. Game Piece Layer
 * Control and Game Piece Layer components of a Map. Within each layer, pieces are also listed in the order they should
 * be drawn (i.e. lowest index at the back/bottom, highest index at the front/top).
 */
public interface PieceCollection {
  /** Reposition a piece to the front of all others in the same visual layer*/
  void moveToFront(GamePiece p);
  /** Reposition a piece to the back of all others in the same visual layer*/
  void moveToBack(GamePiece p);
  /** Return all currently-visible pieces in the collection as a read-only array */
  GamePiece[] getPieces();
  /** Return all pieces in the collection, regardless of visibility */
  GamePiece[] getAllPieces();
  /** Return true if the two pieces can be merged into a single stack */
  boolean canMerge(GamePiece p1, GamePiece p2);
  /**
   * Returns the index of a piece.  When painting the map, pieces
   * are drawn in order of index -- lowest index drawn first and therefore
   * appearing "below" later pieces which are drawn on top of it. */
  int indexOf(GamePiece p);
  /** Removes the piece */
  void remove(GamePiece p);
  /** Adds a piece */
  void add(GamePiece p);
  /** Remove all pieces */
  void clear();
}
