/*
 *
 * Copyright (c) 2021 by The VASSAL Development Team
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
package VASSAL.build.module.map.deck;

import VASSAL.counters.GamePiece;

import java.util.List;

/**
 * A utility class for sorting GamePieces based on a list of sort parameters
 */
public class SortablePiece implements Comparable<SortablePiece> {
  private final GamePiece piece;
  private final List<SortParameter> sortParameters;

  public SortablePiece(List<SortParameter> sortParameters, GamePiece piece) {
    this.sortParameters = sortParameters;
    this.piece = piece;
  }

  public GamePiece getPiece() {
    return piece;
  }

  @Override
  public int compareTo(SortablePiece other) {
    boolean ascending = ! sortParameters.get(0).isDescendingSort();

    if (other == null) {
      return ascending ? -1 : 1;
    }

    // Loop through each level of Sort Parameters
    for (final SortParameter sp : sortParameters) {
      final String sortProperty = sp.getSortProperty();
      ascending = ! sp.isDescendingSort();

      final String otherProperty = (String) other.piece.getProperty(sortProperty);
      if (otherProperty == null) return ascending ? -1 : 1;

      final String myProperty = (String) piece.getProperty(sortProperty);
      if (myProperty == null) return ascending ? 1 : -1;

      // Designer requested a numeric sort, try it if possible.
      if (sp.isNumericSort()) {
        try {
          final Integer otherNum = Integer.parseInt(otherProperty);
          final Integer myNum = Integer.parseInt(myProperty);
          final int result = otherNum.compareTo(myNum);
          if (result == 0) {
            continue; // Equal, Check next level
          }
          else {
            return result * (ascending ? 1 : -1);
          }
        }
        catch (NumberFormatException e) {
          // no action, we revert to string.
        }
      }

      // Designer requested an alpha sort, or the numeric comparison failed.
      final int result = otherProperty.compareTo(myProperty);
      if (result == 0) {
        continue; // Equal, Check next level
      }
      else {
        return result * (ascending ? 1 : -1);
      }

    }

    // Drops through all comparisons, must be equal
    return 0;

  }

  @Override
  public boolean equals(Object o) {
    if (! (o instanceof SortablePiece)) return false;
    return ((SortablePiece)o).piece.equals(piece);
  }
}