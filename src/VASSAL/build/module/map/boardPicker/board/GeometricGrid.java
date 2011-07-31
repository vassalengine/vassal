package VASSAL.build.module.map.boardPicker.board;

import java.awt.Point;
import java.awt.geom.Area;

/*
 * $Id$
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

/**
 * A Map grid that consists of a regular tiling of shapes
 */
public interface GeometricGrid extends MapGrid {
  /**
   * Return the Area representing a set of tiles on the grid
   * @param center the center of the tiles
   * @param range the number of tiles outward from the center to include
   * @return
   */
  public Area getGridShape(Point center, int range);
}
