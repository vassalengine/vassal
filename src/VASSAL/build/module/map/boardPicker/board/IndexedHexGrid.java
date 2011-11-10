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
import java.awt.geom.Point2D;
import java.beans.PropertyChangeListener;

import VASSAL.build.module.map.boardPicker.board.CanonHexIndices.IntegerCHI;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;

public interface IndexedHexGrid {
  public double getDx();
  public double getDy();
  public boolean isSideways();
  public Point getOrigin();
  public GridContainer getContainer();
  public void addPropertyChangeListener(PropertyChangeListener l);
  /** 
   * @return The number of <b>straight</b> hex-rows in the grid.
   */
  public int maxStraight();
  /** 
   * @return The number of <b>oblique</b> hex-rows in the grid. 
   */
  public int maxOblique();
  /** 
   * @return The number of <b>wobble</b> hex-rows in the grid.
   */
  public int maxWobble();
  /* maybe these also */
  public IntegerCHI canonHexIndices(Point2D p);  
  public CanonHexIndices canonHexIndices();
  
  public int range(Point p1, Point p2);
  public Point snapTo(Point p);
//  public Point snapToHex(Point p);
//  public Point snapToHexVertex(Point p);
//  public Point snapToHexSide(Point p) ;
  public Point2D toOrigin(CanonHexIndices chiSrc, Point2D pDst);
}
