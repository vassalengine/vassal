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

import static java.lang.Math.floor;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.HexGridNumberingX;

  /**
   * Canonical indices assume a sideways grid (ie hex flat-sides left and right 
   * and the pointy sides top and bottom) with origin in the upper-left corner, and 
   * track coordinates on the two axes <b>straight</b> (parallels of <b>straight</b>
   * running  horizontal), and <b>oblique</b> (parallels of <b>oblique</b> running
   * bottom-left to top-right).
   * <p>
   * Whenever the <b>wobble</b> index (the other rectangular index, orthogonal
   * to <b>straight</b>) is required it can be obtained from the relationship:<br>
   * <b>wobble = oblique - round( straight / 2.0 +(isStagger ? 0.75 : 0.25) )</b>.<br>
   * However this is properly a GridNumbering property, independent of the HexGrid
   * implementation, due to its dependence on the HexGridNumbering parameter <b>isStagger</b>.
   *       
   * @author Pieter Geerkens
   */
  public interface CanonHexIndices {
    int straight() ;
    int oblique();
    /** Range is the <b>walking</b> distance to another hex.
     * @param c is the CanonHexIndices for the hex for which range from the 
     * current hex is requested.
     * @return The <b>walking</b> distance, in hexes, to the specified hex.  
     */
    int range(CanonHexIndices c);
    public Point  toPoint();
    public Point2D toPoint2D();
    
    /*---------------------- Implementation -----------------------------*/
    @SuppressWarnings("serial")
    public class IntegerCHI extends Point implements CanonHexIndices {
      private IntegerCHI(int straight, int oblique) {
        x = straight; y = oblique;
      }
      @Override
      public  int straight()   { return x; }
      @Override
      public  int oblique()    { return y; }
      
      @Override
      public int range(CanonHexIndices c) {
        return ( Math.abs(c.straight() - x) 
             + Math.abs(c.oblique()  - y) 
             + Math.abs((c.straight()-c.oblique()) - (x - y)) ) / 2;
      }
      @Override
      public Point  toPoint()   { return this; }
      @Override
      public Point2D toPoint2D() { return this;  }
    }
    /*---------------------- CHI Factory -----------------------------*/
    /**
     * 
     * @author Pieter Geerkens
     * @date 12/23/2010
     * @see HexGridX
     * @see HexGridNumberingX
     * @see <a href="http://playtechs.blogspot.com/2007/04/hex-grids.html">Grids</a>
     *  - TODO archive this web-page as documentation
     *  @see <a href = "HexGridAlgorithm.mht">Grids2</a>
     */
    public class CHIFactory {
      private static final Logger logger =
              LoggerFactory.getLogger(CHIFactory.class);
      IndexedHexGrid hexgrid;
      
      public CHIFactory(final IndexedHexGrid hexgrid){
        this.hexgrid = hexgrid;
        hexgrid.addPropertyChangeListener(pclGrid);
//        if (hexgrid.getContainer() instanceof ZonedGrid) {
//          ((AbstractConfigurable)(hexgrid.getContainer()))
//            .addPropertyChangeListener(pclGrid);
//        }
        setContainer(hexgrid.getContainer());
      }
      
      void setContainer(GridContainer container){
        if (container != null)
          if (container.getSize() != null)
          setMaxIndices(container.getSize());
      }
      
      PropertyChangeListener pclGrid = new PropertyChangeListener(){
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          setAffineTransforms(  hexgrid.getDx(),     hexgrid.getDy(),
                        hexgrid.getOrigin(),  hexgrid.isSideways());
          setContainer(hexgrid.getContainer());
        } 
      };
      public CanonHexIndices.IntegerCHI canonHexIndices(Point2D p) {
        return new CanonHexIndices.IntegerCHI(getCoord(affineX, p), getCoord(affineY, p));
      }
      
      private AffineTransform affineX;// Assists in translating (x,y) to straight index
      private AffineTransform affineY;// Assists in translating (x,y) to oblique index
      private AffineTransform affineToScreen; // Transform (straight,oblique) indices to (x,y) coords
      /** Sets the affine transformations required by this HexGrid for translating
       *  between (x,y) coordinates of a point and canonical hex-indices, that being
       *  (straight,oblique) measured from the upper-left corner of the grid.
       *  @param dx x-dimension of the grid at 100% scaling
       *  @param dy y-dimension of the grid at 100% scaling
       *  @param origin <code>Point</code> object for the screen coordinates of the grid origin
       *  @param isSideways Boolean, true if the straight hex-rows run horizontally, else false.
       *  */
      private void setAffineTransforms(double dx, double dy, Point origin, boolean isSideways) {
        final AffineTransform affinePreOp = (isSideways)
                ? new AffineTransform(1,0, 0,1, -origin.y,-origin.x)
                : new AffineTransform(0,1, 1,0, -origin.y,-origin.x);
        
        affineY = new AffineTransform(   2.0/dy,      1.0/dy, 
                               0.0,       (3.0/2.0)/dx, 
                              -0.5,       -0.5);
        affineY.concatenate(affinePreOp);
        
        affineX = new AffineTransform(   1.0/dy,      -1.0/dy, 
                              (3.0/2.0)/dx,   (3.0/2.0)/dx, 
                              -0.5,       -0.5);
        affineX.concatenate(affinePreOp);
        
        affineToScreen = new AffineTransform(-dy/2.0,   dx,
                                  dy,        0.0,
                                  0.0,     0.0 );
        try {
          affinePreOp.invert();
        }
        catch (NoninvertibleTransformException e) {
          logger.error("Non-invertible 'affinePreOp' isn't possible.", e);
        }
        affineToScreen.preConcatenate(affinePreOp);
      }
      
      /**Transforms a <code>CanonHexIndices</code> object into the {x,y) screen
       * coordinates of the center of the hex specified.
       * <p>
       * Behaves identically to <code>java.awt.geom.AffineTransform.transform</code> 
       * @see  java.awt.geom.AffineTransform.transform
       */
      public Point2D affineCHIToScreen(CanonHexIndices pSrc, Point2D pDst){
        return affineToScreen.transform((Point2D)pSrc,pDst);
      }
      public CanonHexIndices canonHexIndices(int straight, int oblique) {
        return new IntegerCHI(straight,oblique);
      }
//      public CanonHexIndices canonHexIndices() {
//        return canonHexIndices(0,0);
//      }
      private int getCoord (AffineTransform affine, Point2D p){
        Point pt = (Point) (affine.transform(p,(new Point())));
        return  (int) floor( (pt.x + pt.y + 2) / 3);
      }

      int maxWobble;
      int maxStraight;
      int maxOblique;
      void setMaxIndices(Dimension size) {
        CanonHexIndices cMax = canonHexIndices(new Point(size.width,size.height));
        CanonHexIndices cMin = canonHexIndices(new Point(0,0));
        maxOblique   = cMax.oblique()   - cMin.oblique();
        maxStraight = cMax.straight() - cMin.straight();
        maxWobble  = maxOblique     - (int)floor((maxStraight+1)/2);
      }
      /** 
       * @return The number of <b>wobble</b> hex-rows in the grid.
       */
      public int maxWobble()   { return maxWobble; }
      /** 
       * @return The number of <b>straight</b> hex-rows in the grid.
       */
      public int maxStraight(){ return maxStraight; }
      /** 
       * @return The number of <b>oblique</b> hex-rows in the grid. 
       */
      public int maxOblique() { return maxOblique; }
    }
}
