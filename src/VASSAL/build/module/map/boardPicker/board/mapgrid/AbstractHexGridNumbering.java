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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import javax.swing.JComponent;
import javax.swing.JPanel;

import VASSAL.build.Buildable;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.map.boardPicker.board.CanonHexIndices;
import VASSAL.build.module.map.boardPicker.board.HexGridX;
import VASSAL.build.module.map.boardPicker.board.mapgrid.WorldHexIndices.WorldHexIndicesFactory;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.counters.Labeler;

public abstract class AbstractHexGridNumbering  extends RegularGridNumbering
implements EnumerableHexGrid, GameComponent {

  public AbstractHexGridNumbering() throws NumberFormatException, 
  IllegalArgumentException, SecurityException {
    super();
  }

  transient protected HexGridX grid;
  transient protected WorldHexIndicesFactory whif;
  
  /* Override wrappers to Configurable Interface */
  /** {@inheritDoc}
   * <p>This implementation is only needed because <code>grid</code> is undefined in super 
   * when it is  called by <code>ImportCustomClass</code> in the Module Editor */
  @Override
  public Configurer getConfigurer() {
    if (grid == null) {
      return null;
    }
    else { 
      return super.getConfigurer();
    }
  }  
  @Override
  public void addTo(Buildable parent) {
    grid  = (HexGridX) parent;
    grid.setGridNumbering(this);
  }
  @Override
  public void removeFrom(Buildable parent) {
    grid.setGridNumbering(null);
  }

  public HexGridX getGrid() { return grid; }
  
  transient protected PropertyChangeListener pclNumbering = new PropertyChangeListener(){
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
      setAffineTransform();
    }
  };
//  public static String getConfigureTypeName(){
//    return "Hex Numbering - Oblique";
//  }

  /** Draw the numbering if visible */
  @Override
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, 
                    double scale, boolean reversed) {
    if (isVisible()) {  forceDraw(g, bounds, visibleRect, scale, reversed); }
  }

  /**
   * Returns the grid indices of a supplied point.
   * @param p is the point on the map grid for the hex to be identified.
   * @return The <b>row</b> and <b>column</b> indices of the hex containing the
   * supplied point, as a <b>WorldIndices</b> object.
   */
  public WorldHexIndices getWorldIndices(Point2D p){
    return whif.toWorldIndices(p);
  }
  /** Returns the name of the hex identified by the world-hex-indices specified. 
   * 
   * @since 3.2.1
   * @author Pieter Geerkens
   */
  String getName(WorldHexIndices wi) {
    return getName(wi.Row(), wi.Col());
  }
  
  /** Draw the numbering, even if not visible */
  public void forceDraw(Graphics g, Rectangle bounds, Rectangle visibleRect, 
                    double scale, boolean reversed) {
    int size = (int) (scale * fontSize + 0.5);
    if (size < 5) {  return;  }  // text too small to read

    Shape oldClip = g.getClip();
    Graphics2D g2d = (Graphics2D) g;
    AffineTransform oldT = g2d.getTransform();
    if (reversed) {
      AffineTransform t = AffineTransform.getRotateInstance(Math.PI, 
          bounds.x + .5 * bounds.width, bounds.y + .5 * bounds.height);
      g2d.transform(t);
      visibleRect = t.createTransformedShape(visibleRect).getBounds();
    }

    if (bounds.intersects(visibleRect)) {
      Rectangle region = bounds.intersection(visibleRect);
  
      if (oldClip != null) {
          Area clipArea = new Area(oldClip);
          clipArea.intersect(new Area(region));
          g.setClip(clipArea);
      }
  
      double deltaX = scale * grid.getDx();
      double deltaY = scale * grid.getDy();
  
      if (grid.isSideways()) {
        bounds = new Rectangle(bounds.y, bounds.x, bounds.height, bounds.width);
        region = new Rectangle(region.y, region.x, region.height, region.width);
      }
  
      int minCol = 2 * (int) Math.floor((region.x - bounds.x - scale * grid.getOrigin().x) 
          / (2 * deltaX));
      double xmin = bounds.x + scale * grid.getOrigin().x + deltaX * minCol;
      double xmax = region.x + region.width + deltaX;
      int minRow = (int) Math.floor((region.y - bounds.y - scale * grid.getOrigin().y) 
          / deltaY);
      double ymin = bounds.y + scale * grid.getOrigin().y + deltaY * minRow;
      double ymax = region.y + region.height + deltaY;
  
      Font f = new Font("Dialog", Font.PLAIN, size);
      Point p = new Point();
      int alignment = Labeler.TOP;
      int offset = -(int) Math.round(deltaY / 2);
//      if (grid.isSideways() || rotateText.valueDegrees != 0) {
      if (grid.isSideways() || rotateText.valueDegrees != 0) {
        alignment = Labeler.CENTER;
        offset = 0;
      }
  
      if (rotateText.valueDegrees != 0) {
        g2d.rotate(rotateText.valueRadians);
      }
  
      // Convert from map co-ordinates to board co-ordinates
      AffineTransform atMapToBrd = AffineTransform.getTranslateInstance(0, - offset);
      atMapToBrd.concatenate(AffineTransform.getTranslateInstance(-bounds.x, -bounds.y ));
  //    atMapToBrd.preConcatenate(grid.isSideways() 
  //            ? new AffineTransform(0.0,1.0, 1.0,0.0, 0.0,0.0) 
  //            : new AffineTransform(1.0,0.0, 0.0,1.0, 0.0,0.0));
      atMapToBrd.concatenate(AffineTransform.getScaleInstance(1/scale,1/scale));
  
      for (double x = xmin; x < xmax; x += 2 * deltaX) {
        for (double y = ymin; y < ymax; y += deltaY) {
          for (int i = 0; i < 2; ++i) {
            p.setLocation(  (int) Math.round(x + deltaX*i),
                      (int) Math.round(y + deltaY*i / 2) + offset);
            grid.rotateIfSideways(p);
            Point centerPoint = offsetLabelCenter(p, scale);
            Labeler.drawLabel(g2d, 
                getName(getWorldIndices(atMapToBrd.transform(p,null))),
                centerPoint.x,  centerPoint.y,
                f,
                Labeler.CENTER,
                alignment, color, null, null);
          }
        }
      }
      if (rotateText.valueDegrees != 0) {
        g2d.rotate(-rotateText.valueRadians);
      }
    }
    g2d.setTransform(oldT);
    g.setClip(oldClip);
  }

  @Override
  protected JComponent getGridVisualizer() {
    if (visualizer == null) {
      visualizer = new JPanel() {
         private static final long serialVersionUID = 1L;
      
         @Override
        public void paint(Graphics g) {
             g.clearRect(0, 0, getWidth(), getHeight());
             Rectangle bounds = new Rectangle(0, 0, getWidth(), getHeight());
             grid.forceDraw(g, bounds, bounds, 1.0, false);
             forceDraw(g, bounds, bounds, 1.0, false);
         }
      
         @Override
        public Dimension getPreferredSize() {
             return new Dimension(3 * (int) grid.getDy(), 3 * (int) grid.getDx());
         }
       };
    }
    return visualizer;
  }
   //----------------------------------------------------------------------------  
  /**
   * Returns the name in the current grid-numbering system for the supplied 
   * point, specified in the coordinate system of the canvas on which the 
   * grid is painted.
   * 
   * @param p are the coordinates of a hex on the hex grid.
   * @return <b>String</b> containing the name of the identified hex.
   * @since 3.2.1
   * @author Pieter Geerkens
   */
  @Override
  public String locationName(Point p) {
    WorldHexIndices wi = whif.toWorldIndices(p);
    
    format.setFormat(locationFormat);
    format.setProperty(GRID_LOCATION, getName(wi.Row(), wi.Col()));
    format.setProperty(ROW, getNamePart(wi.Row()+vOff, vType, vLeading));
    format.setProperty(COLUMN, getNamePart(wi.Col()+hOff, hType, hLeading));
    return format.getLocalizedText();
  }
   //----------------------------------------------------------------------------
  /** Returns the <b>canvas</b> coordinates of the center of the hex specified 
   * by the world indices provided. These indices are interpreted using the
   * current set of World-Coordinate parameters:<br>
   * <b>isVDescending, isHDescending, isOblique, isNw_se</b> and <b>isStaggered</b><br>
   * and of grid parameters:<br>
   * <b>isSideways, HexSize, HexHeight,</b> and <b>container.size</b>..   
   * @param col 
   * @param row
   * @author Pieter Geerkens
   * @see hexCenterPoint
   */
  @Override
  public Point getCenterPoint(int col, int row) {
    try {
      return whif.hexCenterPoint(col,row);
    }
    catch (NoninvertibleTransformException e) {
      // Cannot happen
      throw new IllegalStateException(e);
    }
  }
  
  /** Returns the <b>row</b>-index of the Point <b>p</b>. 
   * @deprecated Use getWorldIndices(p).row instead. 
   * @see getWorldIndices
   */  
  @Deprecated
  @Override
  public int getRow(Point p) {
    return getWorldIndices(new Point(p)).Row();
  }

  /** Returns the <b>column</b>-index of the Point <b>p</b>.
   * @deprecated Use getWorldIndices(p).col instead.  
   * @see getWorldIndices
   */  
  @Deprecated
  @Override
  public int getColumn(Point p) {
    return getWorldIndices(new Point(p)).Col();
  }

  //-------------------- EnumerableHexGrid implementation -------------------
  abstract public void setAffineTransform() ;
  
  @Override
  public CanonHexIndices.IntegerCHI canonHexIndices(Point2D p) {
    return grid.canonHexIndices(p);
  }

  @Override
  public Point2D toOrigin(CanonHexIndices chi){
    return grid.toOrigin(chi, new Point());
  }

  @Override
  public CanonHexIndices canonHexIndices() { return grid.canonHexIndices(); }
  //---------------------- GameComponent implementation ---------------------
  /**
   * As soon as module is fully loaded, ensure that numbering is properly
   * bounded in relation to the enclosing grid.
   */

  @Override
  public void setup(boolean gameStarting) {
    if (gameStarting) setAffineTransform();
  }
  
  @Override
  public Command getRestoreCommand() {
    return null; // No persistent state
  }
}
