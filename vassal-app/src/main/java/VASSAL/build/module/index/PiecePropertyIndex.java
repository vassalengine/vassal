/*
 *
 * Copyright (c) 2023 by The VASSAL Development Team
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
package VASSAL.build.module.index;

import VASSAL.counters.GamePiece;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Maintain a reverse cross-reference of property values to GamePieces that have that value for
 * a given property.
 *
 */
public class PiecePropertyIndex {

  /** Index of the set of pieces that has each value for this property */
  private final Map<String, Set<GamePiece>> pieces = new HashMap<>();

  /** Index of what the currently stored value of each piece is to ensure we can reliably remove
   * old values in case of bugs
   */
  private final Map<String, String> values = new HashMap<>();

  private final String propertyName;

  public PiecePropertyIndex(String propertyName) {
    this.propertyName = propertyName;
  }

  /**
   * Add a new piece to the index, or check and update the value for an existing piece
   *
   * @param piece Piece added/changed
   */
  public void addOrUpdatePiece(GamePiece piece) {

    final String newValue = (String) piece.getProperty(propertyName);

    // Do we know about this piece?
    if (values.containsKey(piece.getId())) {
      final String oldValue = values.get(piece.getId());

      // If no change to value, then do nothing
      if (Objects.equals(newValue, oldValue)) {
        return;
      }

      // Value is changing, remove the old piece from both indexes
      removePiece(piece);
    }

    // Add the new piece to the existing set of pieces with that value
    Set<GamePiece> piecesWithValue = pieces.get(newValue);
    if (piecesWithValue == null) {
      piecesWithValue = new HashSet<>();
    }
    piecesWithValue.add(piece);

    // And update both indices
    pieces.put(newValue, piecesWithValue);
    values.put(piece.getId(), newValue);
  }

  public int getCount() {
    return values.size();
  }
  /**
   * Remove a piece from the index
   *
   * @param piece Piece removed
   */
  public void removePiece(GamePiece piece) {

    // Do we know about this piece?
    if (values.containsKey(piece.getId())) {
      final String oldValue = values.get(piece.getId());

      // Remove this piece from the set of pieces
      final Set<GamePiece> piecesWithValue = pieces.get(oldValue);
      if (piecesWithValue != null) {
        piecesWithValue.remove(piece);
        pieces.put(oldValue, piecesWithValue);
      }

      // Update value index
      values.remove(piece.getId());
    }
  }

  /**
   * Return the set of pieces that have the specified value for the property we are indexing
   *
   * @param propertyValue Value to check
   * @return              Set of pieces that have that value
   */
  public Set<GamePiece> getPieces(String propertyValue) {
    final Set<GamePiece> results = pieces.get(propertyValue);
    return results == null ? new HashSet<>() : results;
  }

}
