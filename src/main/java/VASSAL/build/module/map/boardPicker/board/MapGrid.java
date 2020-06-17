/*
 * $Id$
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
package VASSAL.build.module.map.boardPicker.board;

import java.awt.Point;

import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;

/**
 * A MapGrid overlays a map board to constrain
 * the legal locations of GamePieces
 */
public interface MapGrid {
  /**
   * @return the nearest grid location to the given point
   */
  public Point snapTo(java.awt.Point p);

  /**
   * @return true if the given point may not be a local location.
   * I.e., if this grid will attempt to snap it to the nearest grid location */
  public boolean isLocationRestricted(Point p);

  /**
   * @return a string describing the location containing the given point
   */
  public String locationName(Point p);
  public String localizedLocationName(Point p);

  /**
   * @return A point p such that locationName(p).equals(location).
   * @throws BadCoords if the location is not valid or formatted incorrectly.
   */
  public Point getLocation(String location) throws BadCoords;

  /**
   * @return the range between two points, in some unit appropriate
   * to the grid (e.g. hexes or squares)
   */
  public int range(Point p1, Point p2);


  /** Whether this grid should be drawn on the map */
  public boolean isVisible();

  /**
   * Draw the grid
   * @param bounds the boundaries of the grid (in magnified coordinates)
   * @param scale the magnification factor
   */
  public void draw(java.awt.Graphics g, java.awt.Rectangle bounds, java.awt.Rectangle visibleRect, double scale, boolean reversed);

  public GridNumbering getGridNumbering();

  public static final class BadCoords extends Exception {
    private static final long serialVersionUID = 1L;

    public BadCoords() {
      super();
    }

    public BadCoords(String s) {
      super(s);
    }
  }

}
