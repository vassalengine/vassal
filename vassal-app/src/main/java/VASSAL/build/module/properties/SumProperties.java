/*
 *
 * Copyright (c) 2006 by Rodney Kinney
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

package VASSAL.build.module.properties;

import java.util.Collection;

import VASSAL.counters.GamePiece;

/**
 * For property names of the form sum(name), returns the value of
 * the named property summed over a list of pieces.
 *
 * @author rkinney
 */
public class SumProperties implements PropertySource {
  protected Collection<GamePiece> pieces;

  public SumProperties(Collection<GamePiece> pieces) {
    this.pieces = pieces;
  }

  @Override
  public Object getProperty(Object key) {
    Object value = null;
    final String keyString = key.toString();
    if (keyString.startsWith("sum(") && keyString.endsWith(")")) { //NON-NLS
      final String propertyName = keyString.substring(4, keyString.length() - 1);
      int sum = 0;
      boolean indeterminate = false;
      for (final GamePiece p : pieces) {
        final Object val = p.getLocalizedProperty(propertyName);
        if (val != null) {
          try {
            sum += Integer.parseInt(val.toString());
          }
          catch (final NumberFormatException e) {
          }
        }
        else {
          indeterminate = true;
        }
      }

      if (sum == 0 && indeterminate) {
        value = "?";
      }
      else {
        value = sum + (indeterminate ? "+?" : "");
      }
    }
    else if (keyString.equals("countPieces")) { //NON-NLS
      value = pieces.size();
    }
    else if (!pieces.isEmpty()) {
      value = pieces.iterator().next().getProperty(key);
    }
    return value;
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    return getProperty(key);
  }
}
