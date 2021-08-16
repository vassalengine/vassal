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

import java.util.Comparator;
import java.util.List;

/**
 * Compare 2 GamePieces based on a List of SortParameters
 */
public class SortParameterComparator implements Comparator<GamePiece> {

  private final List<SortParameter> sortParameters;

  public SortParameterComparator(List<SortParameter> sortParameters) {
    this.sortParameters = sortParameters;
  }

  @Override
  public int compare(GamePiece piece1, GamePiece piece2) {
    boolean ascending = ! sortParameters.get(0).isDescendingSort();

    if (piece2 == null) {
      return ascending ? -1 : 1;
    }

    // Loop through each level of Sort Parameters
    for (final SortParameter sp : sortParameters) {
      final String sortProperty = sp.getSortProperty();
      ascending = ! sp.isDescendingSort();

      final String property2 = (String) piece2.getProperty(sortProperty);
      if (property2 == null) return ascending ? -1 : 1;

      final String property1 = (String) piece1.getProperty(sortProperty);
      if (property1 == null) return ascending ? 1 : -1;

      // Designer requested a numeric sort, try it if possible.
      if (sp.isNumericSort()) {
        try {
          final Integer num2 = Integer.parseInt(property2);
          final Integer num1 = Integer.parseInt(property1);
          final int result = num2.compareTo(num1);
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
      final int result = property2.compareTo(property1);
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

}
