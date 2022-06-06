/*
 *
 * Copyright (c) 2005 by Rodney Kinney
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

import java.awt.Point;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.MapGrid;

/**
 * Accepts all pieces within a specified range of a given point on a map
 */
public class RangeFilter implements PieceFilter {
  private final Map map;
  private final Point position;
  private final MapGrid grid;
  private final int range;

  public RangeFilter(Map map, Point location, int range) {
    position = location;
    this.map = map;
    this.range = range;
    final Board b = map.findBoard(location);
    grid = b != null ? b.getGrid() : null;
  }

  @Override
  public boolean accept(GamePiece piece) {
    boolean accept = false;
    if (piece.getMap() == map) {
      final Point pos = piece.getPosition();
      final int theRange = grid != null ? grid.range(position, pos) : (int) Math.round(position.distance(pos));
      accept = theRange <= range;
    }
    return accept;
  }

}
