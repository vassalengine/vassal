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

import java.awt.Color;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.util.HashMap;
import java.util.Map;
import javax.swing.JButton;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.SquareGridNumbering;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.VisibilityCondition;

public class SquareGrid extends AbstractConfigurable implements GeometricGrid, GridEditor.EditableGrid {
  private double dx = 48.0;
  private double dy = 48.0;
  private Point origin = new Point(24, 24);
  private boolean visible = false;
  private boolean edgesLegal = false;
  private boolean cornersLegal = false;
  private boolean dotsVisible = false;
  private Color color;
  protected GridContainer container;
  protected Map shapeCache = new HashMap();
  protected SquareGridEditor gridEditor;

  private GridNumbering gridNumbering;


  public GridNumbering getGridNumbering() {
    return gridNumbering;
  }

  public void setGridNumbering(GridNumbering gridNumbering) {
    this.gridNumbering = gridNumbering;
  }


  public double getDx() {
    return dx;
  }
  
  public void setDx(double d) {
    dx = d;
  }


  public double getDy() {
    return dy;
  }
  
  public void setDy(double d) {
    dy = d;
  }

  public Point getOrigin() {
    return new Point(origin);
  }
  
  public void setOrigin(Point p) {
    origin.x = p.x;
    origin.y = p.y;
  }

  public boolean isSideways() {
    return false;
  }
  
  public void setSideways(boolean b) {
    return;
  }
  
  public GridContainer getContainer() {
    return container;
  }

  public static final String DX = "dx";
  public static final String DY = "dy";
  public static final String X0 = "x0";
  public static final String Y0 = "y0";
  public static final String VISIBLE = "visible";
  public static final String CORNERS = "cornersLegal";
  public static final String EDGES = "edgesLegal";
  public static final String COLOR = "color";
  public static final String DOTS_VISIBLE = "dotsVisible";

  public String[] getAttributeNames() {
    String s[] = {X0, Y0, DX, DY, EDGES, CORNERS, VISIBLE, DOTS_VISIBLE, COLOR};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"X offset",
                        "Y offset",
                        "Cell Width",
                        "Cell Height",
                        "Edges are legal locations",
                        "Corners are legal locations",
                        "Show Grid",
                        "Draw Center Dots",
                        "Color"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Integer.class,
                       Integer.class,
                       Double.class,
                       Double.class,
                       Boolean.class,
                       Boolean.class,
                       Boolean.class,
                       Boolean.class,
                       Color.class};
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return visible;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public void addTo(Buildable b) {
    container = (GridContainer) b;
    container.setGrid(this);
  }

  public void removeFrom(Buildable b) {
    ((GridContainer) b).removeGrid(this);
  }

  public static String getConfigureTypeName() {
    return "Rectangular Grid";
  }

  public String getGridName() {
    return getConfigureTypeName();
  }
  
  public String getConfigureName() {
    return null;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("RectangularGrid.htm");
  }


  public String getAttributeValueString(String key) {
    if (X0.equals(key)) {
      return "" + origin.x;
    }
    else if (Y0.equals(key)) {
      return "" + origin.y;
    }
    else if (DY.equals(key)) {
      return "" + dy;
    }
    else if (DX.equals(key)) {
      return "" + dx;
    }
    else if (CORNERS.equals(key)) {
      return "" + cornersLegal;
    }
    else if (EDGES.equals(key)) {
      return "" + edgesLegal;
    }
    else if (VISIBLE.equals(key)) {
      return "" + visible;
    }
    else if (DOTS_VISIBLE.equals(key)) {
      return "" + dotsVisible;
    }
    else if (COLOR.equals(key)) {
      return visible ? ColorConfigurer.colorToString(color) : null;
    }
    return null;
  }

  public void setAttribute(String key, Object val) {
    if (val == null)
      return;
    if (X0.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      origin.x = ((Integer) val).intValue();
    }
    else if (Y0.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      origin.y = ((Integer) val).intValue();
    }
    else if (DY.equals(key)) {
      if (val instanceof String) {
        val = new Double((String) val);
      }
      dy = ((Double) val).doubleValue();
    }
    else if (DX.equals(key)) {
      if (val instanceof String) {
        val = new Double((String) val);
      }
      dx = ((Double) val).doubleValue();
    }
    else if (CORNERS.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      cornersLegal = ((Boolean) val).booleanValue();
    }
    else if (EDGES.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      edgesLegal = ((Boolean) val).booleanValue();
    }
    else if (VISIBLE.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      visible = ((Boolean) val).booleanValue();
    }
    else if (DOTS_VISIBLE.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      dotsVisible = ((Boolean) val).booleanValue();
    }
    else if (COLOR.equals(key)) {
      if (val instanceof String) {
        val = ColorConfigurer.stringToColor((String) val);
      }
      color = (Color) val;
    }
    shapeCache.clear();
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{SquareGridNumbering.class};
  }

  public Point getLocation(String name) throws MapGrid.BadCoords {
    throw new MapGrid.BadCoords("No naming scheme specified");
  }

  public int range(Point p1, Point p2) {
    return Math.max(Math.abs((int) Math.floor((p2.x - p1.x) / dx + 0.5))
                    , Math.abs((int) Math.floor((p2.y - p1.y) / dy + 0.5)));
  }

  public Area getGridShape(Point center, int range) {
    Area shape = (Area) shapeCache.get(new Integer(range));
    if (shape == null) {
      shape = getSingleSquareShape(0, 0);
      double dx = getDx();
      double dy = getDy();

      for (int x = -range; x < range + 1; x++) {
        int x1 = (int) (x * dx);
//        int yRange = range - Math.abs(x); /* This creates a diamond-shaped range.  Configuration option?  */
        int yRange = range;
        for (int y = -yRange; y < yRange + 1; y++) {
          int y1 = (int) (y * dy);
          shape.add(getSingleSquareShape(x1, y1));
        }
      }
      shapeCache.put(new Integer(range), shape);
    }
    shape = new Area(AffineTransform.getTranslateInstance(center.x, center.y).createTransformedShape(shape));
    return shape;
  }

  /**
   * Return the Shape of a single grid square
   */
  public Area getSingleSquareShape(int centerX, int centerY) {
    double dx = getDx();
    double dy = getDy();
    Rectangle rect = new Rectangle((int) (centerX - dx / 2), (int) (centerY - dy / 2), (int) dx, (int) dy);
    return new Area(rect);
  }

  public Point snapTo(Point p) {
// nx,ny are the closest points to the half-grid
// (0,0) is the center of the origin cell
// (1,0) is the east edge of the origin cell
// (1,1) is the lower-right corner of the origin cell

    int nx = (int) Math.round((p.x - origin.x) / (.5 * dx));
    int ny = (int) Math.round((p.y - origin.y) / (.5 * dy));
    int nx2 = (int) Math.round((p.x - origin.x - .25 * dx) / (.5 * dx));
    int ny2 = (int) Math.round((p.y - origin.y - .25 * dy) / (.5 * dy));


    if (cornersLegal && edgesLegal) {
      ;
    }
    else if (cornersLegal) {
      if (ny % 2 == 0) {  // on a cell center
        nx = 2 * (int) Math.round((float) nx * .5);
      }
      else { // on a corner
        nx = 1 + 2 * (int) Math.round((float) (nx - 1) * .5);
      }
    }
    else if (edgesLegal) {
      if (ny % 2 == 0) {
        if (nx % 2 == 0) { // Cell center
          nx = 2 * (int) Math.round((float) nx * .5);
        }
        else { // Vertical edge
          ;
        }
      }
      else { // Horizontal edge
        nx = 2 * (int) Math.round((float) nx * .5);
      }
    }
    else {
//      nx = 2 * (int) Math.round((float) nx * .5);
//      ny = 2 * (int) Math.round((float) ny * .5);
      nx = 2 * (int) Math.round((float) nx2 * .5);
      ny = 2 * (int) Math.round((float) ny2 * .5);
    }
    return new Point(origin.x + (int) (nx * dx / 2), origin.y + (int) (ny * dy / 2));
  }

  public boolean isLocationRestricted(Point p) {
    return true;
  }

  public String locationName(Point p) {
    return gridNumbering == null ? null : gridNumbering.locationName(p);
  }

  public boolean isVisible() {
    return visible;
  }

  public void setVisible(boolean b) {
    visible = true;
  }
  
  protected void reverse(Point p, Rectangle bounds) {
    p.x = bounds.x + bounds.width - (p.x - bounds.x);
    p.y = bounds.y + bounds.height - (p.y - bounds.y);
  }

  /** Draw the grid, if visible, and accompanying numbering, if set */
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if (visible) {
      forceDraw(g, bounds, visibleRect, scale, reversed);
    }
    if (gridNumbering != null) {
      gridNumbering.draw(g, bounds, visibleRect, scale, reversed);
    }
  }

  /** Draw the grid even if not marked visible */
  public void forceDraw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if (!bounds.intersects(visibleRect)) {
      return;
    }
    if (g instanceof Graphics2D) {
      ((Graphics2D) g).addRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                                            RenderingHints.VALUE_ANTIALIAS_ON));
    }

    Rectangle region = bounds.intersection(visibleRect);

    Shape oldClip = g.getClip();
    if (oldClip != null) {
      Area clipArea = new Area(oldClip);
      clipArea.intersect(new Area(region));
      g.setClip(clipArea);
    }

    double deltaX = scale * dx;
    double deltaY = scale * dy;

    double xmin = reversed ? bounds.x + scale * origin.x + bounds.width - deltaX * Math.round((bounds.x + scale * origin.x + bounds.width - region.x) / deltaX) + deltaX / 2
        : bounds.x + scale * origin.x + deltaX * Math.round((region.x - bounds.x - scale * origin.x) / deltaX) + deltaX / 2;
    double xmax = region.x + region.width;
    double ymin = reversed ? bounds.y + scale * origin.y + bounds.height - deltaY * Math.round((bounds.y + scale * origin.y + bounds.height - region.y) / deltaY) + deltaY / 2
        : bounds.y + scale * origin.y + deltaY * Math.round((region.y - bounds.y - scale * origin.y) / deltaY) + deltaY / 2;
    double ymax = region.y + region.height;

    Point p1 = new Point();
    Point p2 = new Point();
    g.setColor(color == null ? Color.black : color);
    // x is the location of a vertical line
    for (double x = xmin; x < xmax; x += deltaX) {
      p1.move((int) Math.round(x), region.y);
      p2.move((int) Math.round(x), region.y + region.height);
      g.drawLine(p1.x, p1.y, p2.x, p2.y);
    }
    for (double y = ymin; y < ymax; y += deltaY) {
      g.drawLine(region.x, (int) Math.round(y), region.x + region.width, (int) Math.round(y));
    }
    if (dotsVisible) {
      xmin = reversed ? bounds.x + scale * origin.x + bounds.width - deltaX * Math.round((bounds.x + scale * origin.x + bounds.width - region.x) / deltaX)
          : bounds.x + scale * origin.x + deltaX * Math.round((region.x - bounds.x - scale * origin.x) / deltaX);
      ymin = reversed ? bounds.y + scale * origin.y + bounds.height - deltaY * Math.round((bounds.y + scale * origin.y + bounds.height - region.y) / deltaY)
          : bounds.y + scale * origin.y + deltaY * Math.round((region.y - bounds.y - scale * origin.y) / deltaY);
      for (double x = xmin; x < xmax; x += deltaX) {
        for (double y = ymin; y < ymax; y += deltaY) {
          p1.move((int) Math.round(x - .5), (int) Math.round(y - .5));
          g.fillRect(p1.x, p1.y, 2, 2);
        }
      }
    }
    g.setClip(oldClip);
  }
  
  public Configurer getConfigurer() {
    boolean buttonExists = config != null;
    Configurer c = super.getConfigurer();
    if (!buttonExists) {
      JButton b = new JButton("Edit Grid");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          editGrid();
        }
      });
      ((Container) c.getControls()).add(b);
    }
    return c;
  }
  
  public void editGrid() {
  	gridEditor = new SquareGridEditor((GridEditor.EditableGrid) this);
  	gridEditor.setVisible(true);
  	// Local variables may have been updated by GridEditor so refresh
  	// configurers.
  	AutoConfigurer cfg = (AutoConfigurer) getConfigurer();
  	cfg.getConfigurer(DX).setValue(String.valueOf(dx));
  	cfg.getConfigurer(DY).setValue(String.valueOf(dy));
  	cfg.getConfigurer(X0).setValue(String.valueOf(origin.x));
  	cfg.getConfigurer(Y0).setValue(String.valueOf(origin.y));
  }
  
  public class SquareGridEditor extends GridEditor {

    public SquareGridEditor(EditableGrid grid) {
      super(grid);
    }

    /* 
     * Calculate Grid metrics based on three selected points
     */
    public void calculate() {
      if ((isPerpendicular(hp1, hp2) && isPerpendicular(hp1, hp3) && !isPerpendicular(hp2, hp3)) ||
          (isPerpendicular(hp2, hp1) && isPerpendicular(hp2, hp3) && !isPerpendicular(hp1, hp3)) ||
          (isPerpendicular(hp3, hp1) && isPerpendicular(hp3, hp2) && !isPerpendicular(hp1, hp2))) {
        int height = Math.max(Math.abs(hp1.y-hp2.y), Math.abs(hp1.y-hp3.y));
        int width = Math.max(Math.abs(hp1.x-hp2.x), Math.abs(hp1.x-hp3.x));
        int top = Math.min(hp1.y, Math.min(hp2.y, hp3.y));
        int left = Math.min(hp1.x, Math.min(hp2.x, hp3.x));
        grid.setDx(width);
        grid.setDy(height);
        setNewOrigin(new Point(left+width/2, top+height/2));
      }
      else {
        reportShapeError();
      }
      
    }
    
  }
}
