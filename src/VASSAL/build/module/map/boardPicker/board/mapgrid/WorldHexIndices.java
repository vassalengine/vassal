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

package VASSAL.build.module.map.boardPicker.board.mapgrid;

import static java.lang.Math.round;

import java.awt.Point;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;

import VASSAL.build.module.map.boardPicker.board.CanonHexIndices;
import VASSAL.build.module.map.boardPicker.board.IndexedHexGrid;

public interface WorldHexIndices {
  public int Row();
  public int Col();
  public Point2D toPoint2D();
  public Point toPoint();
  @SuppressWarnings("serial")
  /*---------------------------- Implementation ---------------------------*/
  public class IntegerWHI extends Point implements WorldHexIndices {
    private IntegerWHI(Point2D p){ 
      this((int)round(p.getX()), (int)round(p.getY()));
    }
    private IntegerWHI(int col, int row) {
      x = col;
      y = row;
    }
    @Override
    public int Col() { return x; }
    @Override
    public int Row() { return y; }
    @Override
    public Point toPoint()     { return this;  }
    @Override
    public Point2D toPoint2D() { return this; }
  }

  /*----------------------------- WHI Factory ----------------------------*/
  public class WorldHexIndicesFactory {
    final EnumerableHexGrid numbering;

    public WorldHexIndicesFactory(EnumerableHexGrid numbering) {
      this.numbering = numbering;
    }
    
    /** 
     * Transforms the point p to World Indices.
     * 
     * @param p is the coordinates of the point identifying a hex on the map grid.
     * @return <b>WorldCoords</b> storing the row and column indices of the identified hex.
     */
    public IntegerWHI toWorldIndices(Point2D p) {
      return new IntegerWHI(affineToWCS.transform(
          numbering.canonHexIndices(p),null));
    }
    
    /** Returns the <b>canvas</b> coordinates of the center of the hex specified 
     * by the world indices provided. These indices are interpreted using the
     * current set of World-Coordinate parameters:<br>
     * <b>isVDescending, isHDescending, isOblique, isNw_se</b> and <b>isStaggered</b><br>
     * and of grid parameters:<br>
     * <b>isSideways, HexSize, HexHeight,</b> and <b>container.size</b>. 
     * @param whi WorldHexIndices for a specified hex in the <code>HexGrid</code>
     * @Return the screen coordinates for the center of the specified hex
     * @throws NoninvertibleTransformException The controlled construction of the 
     * internal affine transformations should ensure that this cannot be thrown.
     */
    public Point2D hexCenterPoint( WorldHexIndices whi) throws NoninvertibleTransformException{
      CanonHexIndices chi = (CanonHexIndices)affineToWCS.inverseTransform(
          (Point2D) whi, 
          (Point2D)numbering.canonHexIndices()
        );
      return numbering.toOrigin(chi);
    }

    public Point hexCenterPoint(int col, int row) throws NoninvertibleTransformException {
      return (Point) hexCenterPoint(new IntegerWHI(col,row));
    }
    
    Point ToPoint(Point2D p2d){
      return new Point((int)Math.round(p2d.getX()), (int)Math.round(p2d.getY()));
    }

    /** <b>affineToWCS</b> is an affine transform defined by the parameters of a 
     * GridNumbering that converts canonical grid indices, ie (straight,oblique)
     * from the upper left corner, into the corresponding indices in the system
     * defined by the board grid numbering.
     */
    private AffineTransform affineToWCS = AffineTransform.getQuadrantRotateInstance(0);
    
    /** <b>setAffineTransform</b> generates an affine transform that converts 
     * between canonical grid indices and world grid indices. 
    */
    void setAffineTransform(IndexedHexGrid grid,
        boolean isOblique,  boolean isNw_se,     boolean isStagger,
        boolean vDescending, boolean hDescending) {

      boolean isSDescending  = (grid.isSideways() ? vDescending : hDescending );;
      boolean isWODescending = (grid.isSideways() ? hDescending : vDescending );;

      affineToWCS = new AffineTransform();
      if (isOblique) {
        if (isNw_se) affineToWCS.preConcatenate(new AffineTransform(
                   1.0, -1.0,
                   0.0,  1.0,
                   0.0,  grid.maxStraight()/2.0));
        if (isWODescending) affineToWCS.preConcatenate(new AffineTransform(
                   1.0,  0.0,
                   0.0, -1.0,
                   0.0,  grid.maxOblique()));
      }
      else {
        affineToWCS.preConcatenate(new AffineTransform(
                   1.0, -0.5,
                   0.0,  1.0,
                   0.0, -(isStagger?0.75:0.25)));
        if (isWODescending) affineToWCS.preConcatenate(new AffineTransform(
                   1.0,  0.0,
                   0.0, -1.0,
                   0.0,  grid.maxWobble()));
      }
      if (isSDescending) affineToWCS.preConcatenate(new AffineTransform(
                  -1.0,            0.0,  
                   0.0,             1.0, 
                   grid.maxStraight(),  0.0));
    }
    
    void setAffineTransform(double m00, double m10,
                    double m01, double m11) {
      affineToWCS = new AffineTransform(m00, m10, m01, m11, 0, 0);
    }

    void setAffineTransform(double m00, double m10,
                    double m01, double m11,
                    double m02, double m12) {
      affineToWCS = new AffineTransform(m00, m10, m01, m11, m02, m12);
    }
  }
}
