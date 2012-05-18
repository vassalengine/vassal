/*
 * $Id$
 *
 * Copyright (c) 2010-2011 by Pieter Geerkens
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package VASSAL.build.module.map.boardPicker.board;

import java.awt.Point;

/**
 * Dummy wrapper for {@link HexGridX} to facilitate synchronous
 * parallel development.
 * @author Pieter Geerkens
 */
public class HexGrid extends HexGridX {
  public HexGrid() { super(); }

  public HexGrid(double size) { super(size); }

  public HexGrid(double width, double height) { super(width, height); }

  @Deprecated
  public HexGrid(double size, boolean dummyForASL) { super(size); }

  @Deprecated
  public double getHexSize() {
    return getDy();
  }

  @Deprecated
  public double getHexWidth() {
    return getDx();
  }

  @Deprecated
  protected boolean alternate = false;

  @Deprecated
  protected Point origin = new Point(0, 32);

  @Deprecated
  public void setSnapScale(int value) {
    // dummy method
  }

  @Deprecated
  public void setHexWidth(double value) {
    setDx(value);
  }

  @Deprecated
  protected int sideX(int x, int y) {
    return ((int) (dx / 2 * (int) Math.floor((x - origin.x + dx / 4) * 2 / dx) + origin.x));
  }

  @Deprecated
  protected int sideY(int x, int y) {
    final int nx = (int) Math.floor((x - origin.x + dx / 4) * 2 / dx);
    if (nx % 2 == 0) {
      return ((int) (dy / 2 * (int) Math.floor((y - origin.y + dy / 4) * 2 / dy) + origin.y));
    }
    else {
      return ((int) ((dy / 2) * (int) Math.floor((y - origin.y) * 2 / dy) + (int) (dy / 4) + origin.y));
    }
  }

  @Deprecated
  public Point snapToHexVertex(Point p) {
    return super.snapToHexVertex(p);
  }

  @Deprecated
  public void setCornersLegal(boolean cornersLegal_) {
    cornersLegal = cornersLegal_;
  }

  @Deprecated
  public void setEdgesLegal(boolean edgesLegal_) {
    edgesLegal = edgesLegal_;
  }
}
