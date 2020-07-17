/*
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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
package VASSAL.configure;

import VASSAL.counters.BasicPiece;

/** Convenience class that provides prompts for the properties found in a BasicPiece */
public class GamePieceFormattedStringConfigurer extends FormattedStringConfigurer {
  public GamePieceFormattedStringConfigurer(String key, String name) {
    super(key, name);
    String[] allOptions = new String[] {
      BasicPiece.PIECE_NAME,
      BasicPiece.BASIC_NAME,
      BasicPiece.LOCATION_NAME,
      BasicPiece.CURRENT_MAP,
      BasicPiece.CURRENT_BOARD,
      BasicPiece.CURRENT_ZONE};
    setOptions(allOptions);
  }
}
