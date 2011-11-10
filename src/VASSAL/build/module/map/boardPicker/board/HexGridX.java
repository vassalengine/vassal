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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.map.boardPicker.board.CanonHexIndices.*;
import VASSAL.command.Command;

/**
 * Implementation of a hex grid that maintains a canonical (oblique) numbering
 * to facilitate grid-numbering and ranging of hexes.
 * 
 * @author Pieter Geerkens
 *
 */
public class HexGridX extends AbstractUIHexGrid 
implements GameComponent {
  CHIFactory chif;
  
  /** The hexagonal catch buckets for snap-to a center or a corner   */
  HexGridX gridHexVertexBuckets;
  /** The hexagonal catch buckets for snap-to a center or an edge   */
  HexGridX gridHexSideBuckets;
  
  /** 
   * Constructor for an irregular hexagonal grid.
   * @param height  - the height of the grid
   * @param width  - the short-width (ie separation of parallels through centres) of the grid
   */
   public HexGridX(double height, double width) {
     dy = height;
     dx = width;
    chif = new CHIFactory(this);
    setConfigureName(this.getClass().getSimpleName());
  }
   /** 
    * Constructor for a regular (ie equiangular and equilateral) hexagonal grid.
    * <p>The short-width is <i>sqrt(3.0)/2.0</i> times the specified height, and the full-width
    * is <i>2.0/sqrt(3.0)</i> times the specified height.
    * @param height    - height (distance between parallel sides) of the grid
    * @throws NoSuchFieldException 
    */
   public HexGridX(double height) {
      this(height, sqrt3_2 * height);
   }
   /** 
    * Default constructor for a grid of regular hexagons of height 64 (and thus
    * a short-width of about 55.4 and full-width of about 73.9.
    * @throws NoSuchFieldException 
    */
  public HexGridX() {
    this(64.0);
  }
  
  @Override
  public void addTo(Buildable b) {
    super.addTo(b);
    if (GameModule.getGameModule() != null)
      GameModule.getGameModule().getGameState().addGameComponent(this);
     changeSupport.addPropertyChangeListener( new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
        gridHexVertexBuckets.setDy(2.0/3.0*dx);
        gridHexVertexBuckets.setDx(dy/2.0);
        gridHexVertexBuckets.setSideways(!sideways);
        gridHexVertexBuckets.setOrigin(new Point(y0, x0));
        gridHexVertexBuckets.setAttribute(COLOR, new Color(255,0,255, 127)); // MAGENTA

        gridHexSideBuckets.setDy(dy/2.0);
        gridHexSideBuckets.setDx(dx/2.0);
        gridHexSideBuckets.setSideways(sideways);
        gridHexSideBuckets.setOrigin(new Point(x0, y0));
        gridHexSideBuckets.setAttribute(COLOR,  new Color(0,255,255, 127)); // CYAN
       }
    });
     gridHexVertexBuckets = new HexGridX();
     gridHexSideBuckets = new HexGridX();
     changeSupport.firePropertyChange(null,null,null);
  }
  
  @Override
  public HexGridX clone() throws CloneNotSupportedException {
    HexGridX newGrid = (HexGridX) super.clone(); 
    newGrid.setAttribute(DX,dx);
    newGrid.setAttribute(DY, dy);
    newGrid.setAttribute(COLOR, color);
    newGrid.setAttribute(CORNERS, cornersLegal);
    newGrid.setAttribute(DOTS_VISIBLE, dotsVisible);
    newGrid.setAttribute(EDGES, edgesLegal);
    newGrid.setAttribute(SIDEWAYS, sideways);
//    newGrid.setAttribute(SNAP_SCALE, snapScale);
    newGrid.setAttributeTranslatable(VISIBLE, visible);
    newGrid.setAttribute(X0, x0);
    newGrid.setAttribute(Y0, y0);
    return newGrid;
  }
  /*--------------------------------------------------------------------*/
  /* EnumerableHexGrid interface implementation */
  @Override
  public Point2D toOrigin(CanonHexIndices chiSrc, Point2D pDst) {
    return chif.affineCHIToScreen(chiSrc,pDst);
  }
  
  @Override
  public IntegerCHI canonHexIndices(Point2D p) {
    return chif.canonHexIndices(p);
  }
  public CanonHexIndices canonHexIndices(int straight, int oblique) {
    return chif.canonHexIndices(straight,oblique);
  }
  @Override
  public CanonHexIndices canonHexIndices() {
    return chif.canonHexIndices(0,0);
  }
  
  @Override
  public int maxWobble() { return chif.maxWobble(); } 
  @Override
  public int maxStraight() { return chif.maxStraight(); }
  @Override
  public int maxOblique() { return chif.maxOblique(); }
  
  /*--------------------------------------------------------------------*/
  @Override
  public int range(Point p1, Point p2){
    return chif.canonHexIndices(shear.transform(p1,null))
        .range(chif.canonHexIndices(shear.transform(p2,null))); 
  }
    
  @Override
  public Point snapToHex(Point p) {
    return (Point) chif.affineCHIToScreen(chif.canonHexIndices(p),new Point());
  }
  @Override 
  protected Point snapToHexVertex(Point p)  {
    return gridHexVertexBuckets.snapToHex(p);
  }
  @Override 
  public Point snapToHexSide(Point p)  {
    return gridHexSideBuckets.snapToHex(p);
  }
  
  /*--------------------------------------------------------------------*/
  /** Draw the center-vertex catch buckets and/or the center-edge catch buckets, 
   * in red and cyan respectively
   */
  @Override
  public void drawCatchBuckets(Graphics g, Rectangle bounds, Rectangle visibleRect, 
      double zoom, boolean reversed) 
  {
    if (cornersLegal) { gridHexVertexBuckets.forceDraw(g, bounds, visibleRect, zoom, reversed); }
    if (edgesLegal) { gridHexSideBuckets.forceDraw(g, bounds, visibleRect, zoom, reversed); }
  }

  //---------------------- GameComponent implementation ---------------------
  @Override
  public void setup(boolean gameStarting) {
    if (gameStarting) {
      chif.setContainer(container);
      if (numbering != null) {
        if (numbering instanceof GameComponent)
          ((GameComponent)numbering).setup(gameStarting);
      }
    }
  }
  @Override
  public Command getRestoreCommand() {
    return null; // No persistent state
  }
}
