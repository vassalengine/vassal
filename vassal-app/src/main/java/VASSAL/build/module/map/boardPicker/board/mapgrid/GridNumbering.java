/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 21, 2002
 * Time: 10:15:59 PM
 * To change template for new interface use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import java.awt.Point;

import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;

/**
 * Provides methods for assigning names to locations on a MapGrid, and drawing those locations when drawing a grid
 */
public interface GridNumbering {
  public String locationName(Point pt);
  public String localizedLocationName(Point pt);

  public void draw(java.awt.Graphics g, java.awt.Rectangle bounds, java.awt.Rectangle visibleRect, double scale, boolean reversed);

  public boolean isVisible();
  public Point getLocation(String location) throws BadCoords;
}
