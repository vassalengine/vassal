/*
 * Copyright (c) 2021 by Vassalengine.org, Brian Reynolds
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

package VASSAL.build.module.map.boardPicker.board.mapgrid;

import java.awt.Polygon;

public interface PolygonConfigurer {
  void updateCoord(String coordString);
  void updateCoord(int x, int y);

  void updateCoords(Polygon polygon);
  void updateCoords();
}
