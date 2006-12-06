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
import java.awt.Polygon;
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
import VASSAL.build.module.map.boardPicker.board.mapgrid.HexGridNumbering;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.VisibilityCondition;

/**
 * A Hexgrid is a map grid composed of hexes.
 */
public class HexGrid extends AbstractConfigurable implements GeometricGrid, GridEditor.EditableGrid {
  protected Point origin = new Point(0, 32);

  protected double dx;
  protected double dy;

  protected GridContainer container;

  protected GridNumbering numbering;

  protected boolean visible = false;
  protected boolean dotsVisible = false;
  protected boolean edgesLegal = false;
  protected boolean cornersLegal = false;
  protected Color color = Color.black;
  protected boolean sideways = false;
  protected Map shapeCache = new HashMap();
  protected HexGridEditor gridEditor;

  public static final String X0 = "x0";
  public static final String Y0 = "y0";
  public static final String DY = "dy";
  public static final String DX = "dx";
  public static final String VISIBLE = "visible";
  public static final String DOTS_VISIBLE = "dotsVisible";
  public static final String CORNERS = "cornersLegal";
  public static final String EDGES = "edgesLegal";
  public static final String SIDEWAYS = "sideways";
  public static final String COLOR = "color";

  protected static final double sqrt3_2 = Math.sqrt(3) / 2.;

  public String[] getAttributeNames() {
    String s[] = {SIDEWAYS, X0, Y0, DY, DX, EDGES, CORNERS, VISIBLE, DOTS_VISIBLE, COLOR};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Sideways (hexrows go horizontal)",
                        "X offset",
                        "Y offset",
                        "Hex Height",
                        "Hex Width",
                        "Edges are legal locations",
                        "Vertices are legal locations",
                        "Show grid",
                        "Draw center dots",
                        "Color"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{Boolean.class,
                       Integer.class,
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

  public Configurer getConfigurer() {
    
    boolean buttonExists = config != null;
    
    AutoConfigurer c = (AutoConfigurer) super.getConfigurer();
    final Configurer dxConfig = c.getConfigurer(DX);
    c.getConfigurer(DY).addPropertyChangeListener(new java.beans.PropertyChangeListener() {
      public void propertyChange(java.beans.PropertyChangeEvent evt) {
        if (evt.getNewValue() != null) {
          double hgt = ((Double) evt.getNewValue()).doubleValue();
          dxConfig.setValue(new Double(sqrt3_2 * hgt).toString());
        }
      }
    });

    if (!buttonExists) {
      JButton b = new JButton("Edit Grid");
      b.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          editGrid();
        }
      });
      ((Container) c.getControls()).add(b);
    }
    
    return config;
  }


  protected boolean alternate = false;// true if hex B1 is above A1

  public HexGrid(double height, double width, boolean alt) {
    dy = height;
    dx = width;
    alternate = alt;
  }

  public HexGrid(double size, boolean alt) {
    this(size, sqrt3_2 * size, alt);
  }

  public HexGrid() {
    this(64.0, false);
  }

  public boolean isVisible() {
    return visible;
  }

  public boolean isEdgesLegal() {
    return edgesLegal;
  }

  public boolean isCornersLegal() {
    return cornersLegal;
  }

  public void setVisible(boolean legal) {
    visible = legal;
  }

  public void setEdgesLegal(boolean legal) {
    edgesLegal = legal;
  }

  public boolean isSideways() {
    return sideways;
  }
  
  public void setSideways(boolean b) {
    sideways = b;
  }

  public void setCornersLegal(boolean legal) {
    cornersLegal = legal;
  }

  public void setHexSize(double size) {
    dy = size;
    dx = sqrt3_2 * size;
    shapeCache.clear();
  }

  public double getHexSize() {
    return dy;
  }

  public double getHexWidth() {
    return dx;
  }

  public void setHexWidth(double w) {
    dx = w;
  }
  
  public double getDx() {
    return getHexWidth();
  }
  
  public void setDx(double d) {
    setHexWidth(d);
  }
  
  public double getDy() {
    return getHexSize();
  }
  
  public void setDy(double d) {
    dy = d; // DO NOT call setHexSize() so that dx is not reset
  }

  public GridContainer getContainer() {
    return container;
  }

  public void addTo(Buildable b) {
    container = (GridContainer) b;
    container.setGrid(this);
  }

  public void removeFrom(Buildable b) {
    ((GridContainer) b).removeGrid(this);
  }

  public static String getConfigureTypeName() {
    return "Hex Grid";
  }
  
  public String getGridName() {
    return getConfigureTypeName();
  }

  public String getConfigureName() {
    return null;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HexGrid.htm");
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
    else if (SIDEWAYS.equals(key)) {
      return "" + sideways;
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
      if (dx == sqrt3_2 * 64.0) {
        dx = sqrt3_2 * dy;
      }
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
    else if (SIDEWAYS.equals(key)) {
      if (val instanceof String) {
        val = new Boolean((String) val);
      }
      sideways = ((Boolean) val).booleanValue();
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
    return new Class[]{HexGridNumbering.class};
  }

  public String locationName(Point p) {
    return numbering == null ? null : numbering.locationName(p);
  }

  public Point getLocation(String hex) throws MapGrid.BadCoords {
    throw new MapGrid.BadCoords("No naming scheme specified");
  }

  public Point snapTo(Point p) {
    if (edgesLegal && cornersLegal) {
      Point edge = snapToHexSide(p);
      Point vertex = snapToHexVertex(p);
      if ((p.x - edge.x) * (p.x - edge.x)
          + (p.y - edge.y) * (p.y - edge.y)
          < (p.x - vertex.x) * (p.x - vertex.x)
          + (p.y - vertex.y) * (p.y - vertex.y)) {
        return edge;
      }
      else {
        return vertex;
      }
    }
    else if (edgesLegal) {
      return snapToHexSide(p);
    }
    else if (cornersLegal) {
      return snapToHexVertex(p);
    }
    else {
      return snapToHex(p);
    }
  }

  public boolean isLocationRestricted(Point p) {
    return true;
  }

  /**
   * @return the nearest hex center
   */
  public Point snapToHex(Point p) {
    p = new Point(p);
    rotateIfSideways(p);
    p.setLocation(hexX(p.x, p.y), hexY(p.x, p.y));
    rotateIfSideways(p);
    return p;
  }

  /**
   * @return the nearest hex center or hexside
   */
  public Point snapToHexSide(Point p) {
    p = new Point(p);
    rotateIfSideways(p);
    p.setLocation(sideX(p.x, p.y), sideY(p.x, p.y));
    rotateIfSideways(p);
    return p;
  }

  /**
   * @return the nearest hex center or vertex
   */
  public Point snapToHexVertex(Point p) {
    p = new Point(p);
    rotateIfSideways(p);
    p.setLocation(vertexX(p.x, p.y), vertexY(p.x, p.y));
    rotateIfSideways(p);
    return p;
  }

  public void rotate(Point p) {
    int swap = p.x;
    p.x = p.y;
    p.y = swap;
  }

  public void rotateIfSideways(Point p) {
    if (sideways) {
      rotate(p);
    }
  }


  public Area getGridShape(Point center, int range) {
    Area shape = (Area) shapeCache.get(new Integer(range));
    if (shape == null) {
      //Choose a starting point
      Point origin = new Point(0, 0);
      shape = getSingleHexShape(origin.x, origin.y, false);

      for (int i = -range; i <= range; i++) {
        int x = origin.x + (int) (i * dx);

        int length = range * 2 + 1 - Math.abs(i);

        int startY = 0;
        if (length % 2 == 1) {
          startY = origin.y - (int) (dy * (length - 1) / 2);
        }
        else {
          startY = origin.y - (int) (dy * (0.5 + (length - 2) / 2));
        }

        int y = startY;
        for (int j = 0; j < length; j++) {
          Point p = new Point(x, y);
          rotateIfSideways(p);
          shape.add(getSingleHexShape(p.x, p.y, false));
          y += dy;
        }
      }

      rotateIfSideways(origin);
      shape.transform(AffineTransform.getTranslateInstance(0 - origin.x, 0 - origin.y));
      shapeCache.put(new Integer(range), shape);
    }
    shape = new Area(AffineTransform.getTranslateInstance(center.x, center.y).createTransformedShape(shape));
    return shape;
  }

  /**
   * Return the Shape of a single hex
   * @param centerX X co-ord of hex centre
   * @param centerY Y co-ord of hex centre
   * @return Hex Shape
   */
  protected Area getSingleHexShape(int centerX, int centerY, boolean reversed) {
    Polygon poly = new Polygon();

    float x = (float) (sideways ? centerY : centerX);
    float y = (float) (sideways ? centerX : centerY);

    float x1,y1, x2,y2, x3,y3, x4, y4, x5, y5, x6, y6;

    float deltaX = (float) (this.dx);
    float deltaY = (float) (this.dy);

    float r = 2.F * deltaX / 3.F;

    Point p1 = new Point();
    Point p2 = new Point();
    Point p3 = new Point();
    Point p4 = new Point();
    Point p5 = new Point();
    Point p6 = new Point();

    x1 = x - r;
    y1 = y;
    p1.setLocation(Math.round(x1), Math.round(y1));

    x2 = x - .5F * r;
    y2 = reversed ? y + .5F * deltaY : y - .5F * deltaY;
    p2.setLocation(Math.round(x2), Math.round(y2));

    x3 = x + .5F * r;
    y3 = y2;
    p3.setLocation(Math.round(x3) + 1, Math.round(y3));

    x4 = x + r;
    y4 = y;
    p4.setLocation(Math.round(x4) + 1, Math.round(y4));

    x5 = x3;
    y5 = reversed ? y - .5F * deltaY : y + .5F * deltaY;
    p5.setLocation(Math.round(x5) + 1, Math.round(y5) + 1);

    x6 = x2;
    y6 = y5;
    p6.setLocation(Math.round(x6), Math.round(y6) + 1);

    if (sideways) {
      rotate(p1);
      rotate(p2);
      rotate(p3);
      rotate(p4);
      rotate(p5);
      rotate(p6);
    }

    poly.addPoint(p1.x, p1.y);
    poly.addPoint(p2.x, p2.y);
    poly.addPoint(p3.x, p3.y);
    poly.addPoint(p4.x, p4.y);
    poly.addPoint(p5.x, p5.y);
    poly.addPoint(p6.x, p6.y);
    poly.addPoint(p1.x, p1.y);

    return new Area(poly);
  }

  public int range(Point p1, Point p2) {
    p1 = new Point(p1);
    rotateIfSideways(p1);
    p2 = new Point(p2);
    rotateIfSideways(p2);
    int x = p2.x - p1.x;
    int y = p2.y - p1.y;
    double theta = Math.atan2((double) (-x), (double) (-y)) + Math.PI;
    while (theta > Math.PI / 3.)
      theta -= Math.PI / 3.;
    theta = Math.PI / 6. - theta;
    double r = Math.sqrt((double) (x * x + y * y));
    r *= Math.cos(theta);
    return (int) (r / (dy * sqrt3_2) + 0.5);
  }

  protected int hexX(int x, int y) {
    return ((int) (dx * (int) Math.floor((x - origin.x + dx / 2) / dx) + origin.x));
  }

  protected int hexY(int x, int y) {
    int nx = (int) Math.floor((x - origin.x + dx / 2) / dx);
    if (nx % 2 == 0)
      return ((int)
          (dy * (int) Math.floor((y - origin.y + dy / 2) / dy) + origin.y));
    else
      return ((int)
          (dy * (int) Math.floor((y - origin.y) / dy) + (int) (dy / 2) + origin.y));
  }

  protected int sideX(int x, int y) {
    return ((int) (dx / 2 * (int) Math.floor((x - origin.x + dx / 4) * 2 / dx) + origin.x));
  }

  protected int sideY(int x, int y) {
    int nx = (int) Math.floor((x - origin.x + dx / 4) * 2 / dx);
    if (nx % 2 == 0) {
      return ((int) (dy / 2 * (int) Math.floor((y - origin.y + dy / 4) * 2 / dy) + origin.y));
    }
    else {
      return ((int) ((dy / 2) * (int) Math.floor((y - origin.y) * 2 / dy) + (int) (dy / 4) + origin.y));
    }
  }

  protected int vertexX(int x, int y) {
    int ny = (int) Math.floor((y - origin.y + dy / 4) * 2 / dy);
    if (ny % 2 == 0) {
      return ((int) (2 * dx / 3 * (int) (Math.floor(x - origin.x + dx / 3) * 3 / (2 * dx)) + origin.x));
    }
    else {
      return ((int) (2 * dx / 3 * (int) (Math.floor(x - origin.x + dx / 3 + dx / 3) * 3 / (2 * dx))
          - (int) (dx / 3) + origin.x));
    }
  }

  protected int vertexY(int x, int y) {
    return ((int) (dy / 2 * (int) Math.floor((y - origin.y + dy / 4) * 2 / dy) + origin.y));
  }

  /** Draw the grid, if visible, and the accompanying numbering */
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double zoom, boolean reversed) {
    if (visible) {
      forceDraw(g, bounds, visibleRect, zoom, reversed);
    }
    if (numbering != null) {
      numbering.draw(g, bounds, visibleRect, zoom, reversed);
    }
  }

  /** Draw the grid even if set to be not visible */
  public void forceDraw(Graphics g, Rectangle bounds, Rectangle visibleRect, double zoom, boolean reversed) {
    if (!bounds.intersects(visibleRect)) {
      return;
    }
    if (g instanceof Graphics2D) {
      ((Graphics2D) g).addRenderingHints(new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                                            RenderingHints.VALUE_ANTIALIAS_ON));
    }

    g.setColor(color == null ? Color.black : color);

    float x1,y1, x2,y2, x3,y3, x4, y4;

    float deltaX = (float) (this.dx * zoom);
    float deltaY = (float) (this.dy * zoom);

    float r = 2.F * deltaX / 3.F;

    Rectangle region = bounds.intersection(visibleRect);

    Shape oldClip = g.getClip();
    if (oldClip != null) {
      Area clipArea = new Area(oldClip);
      clipArea.intersect(new Area(region));
      g.setClip(clipArea);
    }

    if (sideways) {
      bounds = new Rectangle(bounds.y, bounds.x, bounds.height, bounds.width);
      region = new Rectangle(region.y, region.x, region.height, region.width);
    }

    float xmin = reversed ? bounds.x + (float) zoom * origin.x + bounds.width - 2 * deltaX * (float) Math.ceil((bounds.x + zoom * origin.x + bounds.width - region.x) / (2 * deltaX))
        : bounds.x + (float) zoom * origin.x + 2 * deltaX * (float) Math.floor((region.x - bounds.x - zoom * origin.x) / (2 * deltaX));
    float xmax = region.x + region.width + 2 * deltaX;
    float ymin = reversed ? bounds.y + (float) zoom * origin.y + bounds.height - deltaY * (float) Math.ceil((bounds.y + zoom * origin.y + bounds.height - region.y) / deltaY)
        : bounds.y + (float) zoom * origin.y + deltaY * (float) Math.floor((region.y - bounds.y - zoom * origin.y) / deltaY);
    float ymax = region.y + region.height + deltaY;

    Point center = new Point();
    Point p1 = new Point();
    Point p2 = new Point();
    Point p3 = new Point();
    Point p4 = new Point();

    // x,y is the center of a hex
    for (float x = xmin; x < xmax; x += zoom * 2 * dx) {
      for (float y = ymin; y < ymax; y += zoom * dy) {
        x1 = x - r;
        y1 = y;
        p1.setLocation(Math.round(x1), Math.round(y1));
        x2 = x - .5F * r;
        y2 = reversed ? y + .5F * deltaY : y - .5F * deltaY;
        p2.setLocation(Math.round(x2), Math.round(y2));
        x3 = x + .5F * r;
        y3 = y2;
        p3.setLocation(Math.round(x3), Math.round(y3));
        x4 = x + r;
        y4 = y;
        p4.setLocation(Math.round(x4), Math.round(y4));
        if (sideways) {
          rotate(p1);
          rotate(p2);
          rotate(p3);
          rotate(p4);
        }
        g.drawLine(p1.x, p1.y, p2.x, p2.y);
        g.drawLine(p2.x, p2.y, p3.x, p3.y);
        g.drawLine(p3.x, p3.y, p4.x, p4.y);
        if (dotsVisible) {
          center.setLocation(Math.round(x), Math.round(y));
          rotateIfSideways(center);
          g.fillRect(center.x, center.y, 2, 2);
          center.setLocation(Math.round(x + deltaX), Math.round(y + deltaY / 2));
          rotateIfSideways(center);
          g.fillRect(center.x, center.y, 2, 2);
        }
        x1 += deltaX;
        x2 += deltaX;
        x3 += deltaX;
        x4 += deltaX;
        y1 += .5F * deltaY;
        y2 += .5F * deltaY;
        y3 += .5F * deltaY;
        y4 += .5F * deltaY;
        p1.setLocation(Math.round(x1), Math.round(y1));
        p2.setLocation(Math.round(x2), Math.round(y2));
        p3.setLocation(Math.round(x3), Math.round(y3));
        p4.setLocation(Math.round(x4), Math.round(y4));
        if (sideways) {
          rotate(p1);
          rotate(p2);
          rotate(p3);
          rotate(p4);
        }
        g.drawLine(p1.x, p1.y, p2.x, p2.y);
        g.drawLine(p2.x, p2.y, p3.x, p3.y);
        g.drawLine(p3.x, p3.y, p4.x, p4.y);
        if (x == xmin) {
          p1.setLocation(Math.round(x - r), Math.round(y));
          p2.setLocation(Math.round(x - r / 2), Math.round(y + deltaY / 2));
          if (sideways) {
            rotate(p1);
            rotate(p2);
          }
          g.drawLine(p1.x, p1.y, p2.x, p2.y);
        }
      }
    }
    g.setClip(oldClip);
  }

  public void setGridNumbering(GridNumbering numbering) {
    this.numbering = numbering;
  }


  public GridNumbering getGridNumbering() {
    return numbering;
  }


  public Point getOrigin() {
    return new Point(origin);
  }
  
  public void setOrigin(Point p) {
    origin.x = p.x;
    origin.y = p.y;
  }
  
  public void editGrid() {
  	gridEditor = new HexGridEditor((GridEditor.EditableGrid) this);
  	gridEditor.setVisible(true);
  	// Local variables may have been updated by GridEditor so refresh
  	// configurers. Setting the Dy configurer will auto-recalculate dx
 	double origDx = dx;
  	AutoConfigurer cfg = (AutoConfigurer) getConfigurer();
  	cfg.getConfigurer(DY).setValue(String.valueOf(dy));
  	dx = origDx;
  	cfg.getConfigurer(DX).setValue(String.valueOf(dx));
  	cfg.getConfigurer(X0).setValue(String.valueOf(origin.x));
  	cfg.getConfigurer(Y0).setValue(String.valueOf(origin.y));
  	cfg.getConfigurer(SIDEWAYS).setValue(String.valueOf(sideways));
  }
  
  public class HexGridEditor extends GridEditor {

    public HexGridEditor(EditableGrid grid) {
      super(grid);
    }

    /*
     * Calculate approximate grid metrics based on the three adjacent points
     * picked out by the user.
     */
    public void calculate() {
      
      /*
       * Two of the points must lie on the same horizontal or vertical line (be perpendicular to). 
       * The third point must not be perpendicular to either of the first two. First step is to work out
       * which is which as we can't be sure what order they picked out the points in.
       */
      
      if (isPerpendicular(hp1, hp2)) {
        calculate_step2(hp1, hp2, hp3);
      }
      else if (isPerpendicular(hp1, hp3)) {
        calculate_step2(hp1, hp3, hp2);
      }
      else if (isPerpendicular(hp2, hp3)) {
        calculate_step2(hp2, hp3, hp1);
      }
      else {
        reportShapeError();
      }
    }
    
    /*
     * Step 2. Check third point is not perpendicular to either of
     * the first two, then call appropriate calculation routine 
     * depending on location relative to the first two.
     */
    protected void calculate_step2(Point p1, Point p2, Point p3) {
      if (!isPerpendicular(p1, p3) && !isPerpendicular(p2, p3)) {
        if (isHorizontal(p1, p2)) {
          if ((p3.x < p1.x && p3.x < p2.x) ||(p3.x > p1.x && p3.x > p2.x)) {
            check(false, p1, p2, p3);
          }
          else {
            checkEnd(true, p1, p2, p3);
          }
        }
        else {
          if ((p3.y < p1.y && p3.y < p2.y) ||(p3.y > p1.y && p3.y > p2.y)) {
            check(true, reverse(p1), reverse(p2), reverse(p3));
          }
          else {
            checkEnd(false, reverse(p1), reverse(p2), reverse(p3));
          }
        }
      }
      else {
        reportShapeError();
      }
    }

    protected Point reverse (Point p) {
      return new Point(p.y, p.x);
    }

    protected void check (boolean sideways, Point p1, Point p2, Point p3) {
      
      int r = Math.abs(p1.x - p2.x);
      int width = r * 3 / 2;
      int height = Math.abs(p3.y - p2.y) * 2;
      
      int Xoff = (Math.min(p1.x, p2.x)) % width + (int) (r/2);
      int col = (int) (Math.min(p1.x, p2.x) / width);
      int Yoff = Math.min(p1.y, p2.y) % height - ((col % 2 == 1) ? 0 : (int) (height / 2));
      if (Yoff < 0) Yoff += height;

      setMetrics(width, height, Xoff, Yoff, sideways);
    }
    
    protected void checkEnd (boolean sideways, Point p1, Point p2, Point p3) {
      if (Math.abs((p1.x + p2.x) / 2 - p3.x) > ERROR_MARGIN) {
        reportShapeError();
        return;
      }
      
      int r = Math.abs(p3.y - p1.y) * 2;
      int width = r * 3 / 2;
      int height = Math.abs(p3.x - p2.x) * 2;
      
      int xOrigin = p1.y - (p3.y < p1.y ? 0 : r);
      int Xoff = xOrigin % width + (int) (r/2);
      int col = (int) (xOrigin / width);
      int Yoff = Math.min(p1.x, p2.x) % height - ((col % 2 == 1) ? 0 : (int) (height / 2));
      
      setMetrics(width, height, Xoff, Yoff, sideways);
    }
    
    protected void setMetrics(int width, int height, int xoff, int yoff, boolean b) {
      
      grid.setDx(width);
      grid.setDy(height);
      grid.setOrigin(new Point(xoff, yoff));
      grid.setSideways(b);

    }
    
  }
}
