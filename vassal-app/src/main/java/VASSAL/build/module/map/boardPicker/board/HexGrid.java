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
package VASSAL.build.module.map.boardPicker.board;

import static java.lang.Math.PI;
import static java.lang.Math.abs;
import static java.lang.Math.atan2;
import static java.lang.Math.ceil;
import static java.lang.Math.cos;
import static java.lang.Math.floor;
import static java.lang.Math.max;
import static java.lang.Math.min;
import static java.lang.Math.round;
import static java.lang.Math.sqrt;

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
import VASSAL.i18n.Resources;

/**
 * A Hexgrid is a map grid composed of hexes.
 */
public class HexGrid extends AbstractConfigurable
                     implements GeometricGrid,
                                GridEditor.EditableGrid {
  protected Point origin = new Point(0, 32);

  protected double dx;
  protected double dy;
  protected int snapScale = 0;

  protected GridContainer container;

  protected GridNumbering numbering;

  protected boolean visible = false;
  protected boolean dotsVisible = false;
  protected boolean edgesLegal = false;
  protected boolean cornersLegal = false;
  protected Color color = Color.black;
  protected boolean sideways = false;
  protected boolean snapTo = true;
  protected Map<Integer,Area> shapeCache = new HashMap<>();
  protected HexGridEditor gridEditor;

  public static final String X0 = "x0"; //$NON-NLS-1$
  public static final String Y0 = "y0"; //$NON-NLS-1$
  public static final String DY = "dy"; //$NON-NLS-1$
  public static final String DX = "dx"; //$NON-NLS-1$
  public static final String VISIBLE = "visible"; //$NON-NLS-1$
  public static final String DOTS_VISIBLE = "dotsVisible"; //$NON-NLS-1$
  public static final String CORNERS = "cornersLegal"; //$NON-NLS-1$
  public static final String EDGES = "edgesLegal"; //$NON-NLS-1$
  public static final String SIDEWAYS = "sideways"; //$NON-NLS-1$
  public static final String COLOR = "color"; //$NON-NLS-1$
  public static final String SNAP_SCALE = "snapscale"; //$NON-NLS-1$
  public static final String SNAP_TO = "snapTo"; //$NON-NLS-1$

  protected static final double sqrt3_2 = sqrt(3) / 2.;

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      SIDEWAYS,
      X0,
      Y0,
      DY,
      DX,
      SNAP_TO,
      EDGES,
      CORNERS,
      VISIBLE,
      DOTS_VISIBLE,
      COLOR
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.HexGrid.sideways"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.x_offset"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.y_offset"), //$NON-NLS-1$
      Resources.getString("Editor.HexGrid.hex_height"), //$NON-NLS-1$
      Resources.getString("Editor.HexGrid.hex_width"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.snap"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.edges"), //$NON-NLS-1$
      Resources.getString("Editor.HexGrid.vertices"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.show_grid"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.center_dots"), //$NON-NLS-1$
      Resources.getString(Resources.COLOR_LABEL),
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      Boolean.class,
      Integer.class,
      Integer.class,
      Double.class,
      Double.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      Boolean.class,
      Color.class
    };
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLOR.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return visible;
        }
      };
    }
    else if (EDGES.equals(name) || CORNERS.equals(name)) {
      return new VisibilityCondition() {
        @Override
        public boolean shouldBeVisible() {
          return snapTo;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  @Override
  public Configurer getConfigurer() {

    boolean buttonExists = config != null;

    AutoConfigurer c = (AutoConfigurer) super.getConfigurer();
    final Configurer dxConfig = c.getConfigurer(DX);
    c.getConfigurer(DY).addPropertyChangeListener(new java.beans.PropertyChangeListener() {
      @Override
      public void propertyChange(java.beans.PropertyChangeEvent evt) {
        if (evt.getNewValue() != null) {
          double hgt = (Double) evt.getNewValue();
          dxConfig.setValue(Double.valueOf(sqrt3_2 * hgt).toString());
        }
      }
    });

    if (!buttonExists) {
      JButton b = new JButton(Resources.getString("Editor.Grid.edit_grid")); //$NON-NLS-1$
      b.addActionListener(new ActionListener() {
        @Override
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

  @Override
  public boolean isVisible() {
    return visible == true || (numbering != null && numbering.isVisible());
  }

  public boolean isEdgesLegal() {
    return edgesLegal;
  }

  public boolean isCornersLegal() {
    return cornersLegal;
  }

  @Override
  public void setVisible(boolean legal) {
    visible = legal;
  }

  public void setEdgesLegal(boolean legal) {
    edgesLegal = legal;
  }

  @Override
  public boolean isSideways() {
    return sideways;
  }

  @Override
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

  @Override
  public double getDx() {
    return getHexWidth();
  }

  @Override
  public void setDx(double d) {
    setHexWidth(d);
  }

  @Override
  public double getDy() {
    return getHexSize();
  }

  @Override
  public void setDy(double d) {
    dy = d; // DO NOT call setHexSize() so that dx is not reset
  }

  @Override
  public GridContainer getContainer() {
    return container;
  }

  @Override
  public void addTo(Buildable b) {
    container = (GridContainer) b;
    container.setGrid(this);
  }

  @Override
  public void removeFrom(Buildable b) {
    ((GridContainer) b).removeGrid(this);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.HexGrid.component_type"); //$NON-NLS-1$
  }

  @Override
  public String getGridName() {
    return getConfigureTypeName();
  }

  @Override
  public String getConfigureName() {
    return null;
  }

  @Override
  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HexGrid.htm"); //$NON-NLS-1$
  }

  @Override
  public String getAttributeValueString(String key) {
    if (X0.equals(key)) {
      return String.valueOf(origin.x);
    }
    else if (Y0.equals(key)) {
      return String.valueOf(origin.y);
    }
    else if (DY.equals(key)) {
      return String.valueOf(dy);
    }
    else if (DX.equals(key)) {
      return String.valueOf(dx);
    }
    else if (SNAP_TO.equals(key)) {
      return String.valueOf(snapTo);
    }
    else if (CORNERS.equals(key)) {
      return String.valueOf(cornersLegal);
    }
    else if (EDGES.equals(key)) {
      return String.valueOf(edgesLegal);
    }
    else if (SIDEWAYS.equals(key)) {
      return String.valueOf(sideways);
    }
    else if (VISIBLE.equals(key)) {
      return String.valueOf(visible);
    }
    else if (DOTS_VISIBLE.equals(key)) {
      return String.valueOf(dotsVisible);
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    return null;
  }

  @Override
  public void setAttribute(String key, Object val) {
    if (X0.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String) val);
      }
      origin.x = (Integer) val;
    }
    else if (Y0.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String) val);
      }
      origin.y = (Integer) val;
    }
    else if (DY.equals(key)) {
      if (val instanceof String) {
        val = Double.valueOf((String) val);
      }
      dy = (Double) val;
      if (dx == sqrt3_2 * 64.0) {
        dx = sqrt3_2 * dy;
      }
    }
    else if (DX.equals(key)) {
      if (val instanceof String) {
        val = Double.valueOf((String) val);
      }
      dx = (Double) val;
    }
    else if (SNAP_TO.equals(key)) {
      if (val instanceof String) {
        val = Boolean.valueOf((String) val);
      }
      snapTo = (Boolean) val;
    }
    else if (CORNERS.equals(key)) {
      if (val instanceof String) {
        val = Boolean.valueOf((String) val);
      }
      cornersLegal = (Boolean) val;
    }
    else if (EDGES.equals(key)) {
      if (val instanceof String) {
        val = Boolean.valueOf((String) val);
      }
      edgesLegal = (Boolean) val;
    }
    else if (SIDEWAYS.equals(key)) {
      if (val instanceof String) {
        val = Boolean.valueOf((String) val);
      }
      sideways = (Boolean) val;
    }
    else if (VISIBLE.equals(key)) {
      if (val instanceof String) {
        val = Boolean.valueOf((String) val);
      }
      visible = (Boolean) val;
    }
    else if (DOTS_VISIBLE.equals(key)) {
      if (val instanceof String) {
        val = Boolean.valueOf((String) val);
      }
      dotsVisible = (Boolean) val;
    }
    else if (COLOR.equals(key)) {
      if (val instanceof String) {
        val = ColorConfigurer.stringToColor((String) val);
      }
      color = (Color) val;
    }
    else if (SNAP_SCALE.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String)val);
      }
      snapScale = (Integer)val;
    }
    shapeCache.clear();
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{HexGridNumbering.class};
  }

  @Override
  public String locationName(Point p) {
    return numbering == null ? null : numbering.locationName(p);
  }

  @Override
  public String localizedLocationName(Point p) {
    return numbering == null ? null : numbering.localizedLocationName(p);
  }

  @Override
  public Point getLocation(String location) throws BadCoords {
    if (numbering == null)
      throw new BadCoords();
    else
      return numbering.getLocation(location);
  }

  @Override
  public Point snapTo(Point p) {
    if (! snapTo) {
      return p;
    }
    Point center = snapToHex(p);

    if (edgesLegal && cornersLegal) {
      Point edge = snapToHexSide(p);
      Point vertex = snapToHexVertex(p);
      if ((p.x - edge.x) * (p.x - edge.x)
          + (p.y - edge.y) * (p.y - edge.y)
          < (p.x - vertex.x) * (p.x - vertex.x)
          + (p.y - vertex.y) * (p.y - vertex.y)) {
        return checkCenter(center, edge);
      }
      else {
        return checkCenter(center, vertex);
      }
    }
    else if (edgesLegal) {
      return checkCenter(center, snapToHexSide(p));
    }
    else if (cornersLegal) {
      return checkCenter(center, snapToHexVertex(p));
    }
    else {
      return snapToHex(p);
    }
  }

  // FIXME: snapToHexVertex() does not always return the correct X co-ordinate
  // if the point is close to the center of the Hex. Workaround by returning
  // the real hex center if it is within 1 pixel x/y
  protected Point checkCenter(Point center, Point target) {
    if ((center.x - target.x) * (center.x - target.x)
          + (center.y - target.y) * (center.y - target.y) <= 2) {
      return center;
    }
    else {
      return target;
    }
  }


  @Override
  public boolean isLocationRestricted(Point p) {
    return snapTo;
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
    int x = sideX(p.x, p.y);
    int y = sideY(p.x, p.y);
    if (snapScale > 0) {
      int hexX = hexX(p.x,p.y);
      int hexY = hexY(p.x,p.y);
      if (abs(p.x-hexX) + abs(p.y-hexY) <= abs(p.x-x)+abs(p.y-y)) {
        x = hexX;
        y = hexY;
      }
    }
    p.setLocation(x, y);
    rotateIfSideways(p);
    return p;
  }

  /**
   * @return the nearest hex center or vertex
   */
  public Point snapToHexVertex(Point p) {
    p = new Point(p);
    rotateIfSideways(p);
    int x = vertexX(p.x, p.y);
    int y = vertexY(p.x, p.y);
    if (snapScale > 0) {
      int hexX = hexX(p.x,p.y);
      int hexY = hexY(p.x,p.y);
      if (abs(p.x-hexX) + abs(p.y-hexY) <= abs(p.x-x)+abs(p.y-y)) {
        x = hexX;
        y = hexY;
      }
    }
    p.setLocation(x, y);
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


  @Override
  public Area getGridShape(Point center, int range) {
    Area shape = shapeCache.get(range);
    if (shape == null) {
      //Choose a starting point
      Point origin = new Point(0, 0);
      shape = getSingleHexShape(origin.x, origin.y, false);

      for (int i = -range; i <= range; i++) {
        int x = origin.x + (int) (i * dx);

        int length = range * 2 + 1 - abs(i);

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
      shape.transform(
        AffineTransform.getTranslateInstance(0 - origin.x, 0 - origin.y));
      shapeCache.put(range, shape);
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

    float x = (sideways ? centerY : centerX);
    float y = (sideways ? centerX : centerY);

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
    p1.setLocation(round(x1), round(y1));

    x2 = x - 0.5F * r;
    y2 = reversed ? y + 0.5F * deltaY : y - 0.5F * deltaY;
    p2.setLocation(round(x2), round(y2));

    x3 = x + 0.5F * r;
    y3 = y2;
    p3.setLocation(round(x3) + 1, round(y3));

    x4 = x + r;
    y4 = y;
    p4.setLocation(round(x4) + 1, round(y4));

    x5 = x3;
    y5 = reversed ? y - 0.5F * deltaY : y + 0.5F * deltaY;
    p5.setLocation(round(x5) + 1, round(y5) + 1);

    x6 = x2;
    y6 = y5;
    p6.setLocation(round(x6), round(y6) + 1);

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

  @Override
  public int range(Point p1, Point p2) {
    p1 = new Point(p1);
    rotateIfSideways(p1);
    p2 = new Point(p2);
    rotateIfSideways(p2);
    int x = p2.x - p1.x;
    int y = p2.y - p1.y;
    double theta = atan2((-x), (-y)) + PI;
    while (theta > PI / 3.)
      theta -= PI / 3.;
    theta = PI / 6. - theta;
    double r = sqrt((x * x + y * y));
    r *= cos(theta);
    return (int) (r / (dy * sqrt3_2) + 0.5);
  }

  protected int hexX(int x, int y) {
    int loc = ((int) (dx * (int) floor((x - origin.x + dx / 2) / dx) + origin.x));
    if (snapScale > 0) {
      int delta = x - loc;
      delta = (int)round(delta/(0.5*dx/snapScale));
      delta = max(delta,1-snapScale);
      delta = min(delta,snapScale-1);
      delta = (int)round(delta*0.5*dx/snapScale);
      loc += delta;
    }
    return loc;
  }

  protected int hexY(int x, int y) {
    int nx = (int) floor((x - origin.x + dx / 2) / dx);
    int loc;
    if (nx % 2 == 0)
      loc = ((int)
          (dy * (int) floor((y - origin.y + dy / 2) / dy) + origin.y));
    else
      loc = ((int)
          (dy * (int) floor((y - origin.y) / dy) + (int) (dy / 2) + origin.y));
    if (snapScale > 0) {
      int delta = y - loc;
      delta = (int)round(delta/(0.5*dy/snapScale));
      delta = max(delta,1-snapScale);
      delta = min(delta,snapScale-1);
      delta = (int)round(delta*0.5*dy/snapScale);
      loc += delta;
    }
    return loc;
  }

  protected int sideX(int x, int y) {
    return ((int) (dx / 2 * (int) floor((x - origin.x + dx / 4) * 2 / dx) + origin.x));
  }

  protected int sideY(int x, int y) {
    int nx = (int) floor((x - origin.x + dx / 4) * 2 / dx);
    if (nx % 2 == 0) {
      return ((int) (dy / 2 * (int) floor((y - origin.y + dy / 4) * 2 / dy) + origin.y));
    }
    else {
      return ((int) ((dy / 2) * (int) floor((y - origin.y) * 2 / dy) + (int) (dy / 4) + origin.y));
    }
  }

  // FIXME: vertexX does not always return the same value as HexX for hex
  // centres, it is sometimes 1 pixel off. The values returned for the
  // vertices are fine, so snapTo() has been changed to work around this.
  // There is a rounding error in here if someone else wants to track it down.
  protected int vertexX(int x, int y) {
    int ny = (int) floor((y - origin.y + dy / 4) * 2 / dy);
    if (ny % 2 == 0) {
      return ((int) (2 * dx / 3 * (int) (floor(x - origin.x + dx / 3) * 3 / (2 * dx)) + origin.x));
    }
    else {
      return ((int) (2 * dx / 3 * (int) (floor(x - origin.x + dx / 3 + dx / 3) * 3 / (2 * dx))
          - (int) (dx / 3) + origin.x));
    }
  }

  protected int vertexY(int x, int y) {
    return ((int) (dy / 2 * (int) floor((y - origin.y + dy / 4) * 2 / dy) + origin.y));
  }

  /** Draw the grid, if visible, and the accompanying numbering */
  @Override
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
    if (!bounds.intersects(visibleRect) || color == null) {
      return;
    }

    Graphics2D g2d = (Graphics2D) g;

    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);

    g2d.setColor(color);

    float x1,y1, x2,y2, x3,y3, x4, y4;

    float deltaX = (float) (this.dx * zoom);
    float deltaY = (float) (this.dy * zoom);

    float r = 2.F * deltaX / 3.F;

    Rectangle region = bounds.intersection(visibleRect);

    Shape oldClip = g2d.getClip();
    if (oldClip != null) {
      Area clipArea = new Area(oldClip);
      clipArea.intersect(new Area(region));
      g2d.setClip(clipArea);
    }

    if (sideways) {
      bounds = new Rectangle(bounds.y, bounds.x, bounds.height, bounds.width);
      region = new Rectangle(region.y, region.x, region.height, region.width);
    }

    float xmin = reversed ? bounds.x + (float) zoom * origin.x + bounds.width - 2 * deltaX * (float) ceil((bounds.x + zoom * origin.x + bounds.width - region.x) / (2 * deltaX))
        : bounds.x + (float) zoom * origin.x + 2 * deltaX * (float) floor((region.x - bounds.x - zoom * origin.x) / (2 * deltaX));
    float xmax = region.x + region.width + 2 * deltaX;
    float ymin = reversed ? bounds.y + (float) zoom * origin.y + bounds.height - deltaY * (float) ceil((bounds.y + zoom * origin.y + bounds.height - region.y) / deltaY)
        : bounds.y + (float) zoom * origin.y + deltaY * (float) floor((region.y - bounds.y - zoom * origin.y) / deltaY);
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
        p1.setLocation(round(x1), round(y1));
        x2 = x - 0.5F * r;
        y2 = reversed ? y + 0.5F * deltaY : y - 0.5F * deltaY;
        p2.setLocation(round(x2), round(y2));
        x3 = x + 0.5F * r;
        y3 = y2;
        p3.setLocation(round(x3), round(y3));
        x4 = x + r;
        y4 = y;
        p4.setLocation(round(x4), round(y4));
        if (sideways) {
          rotate(p1);
          rotate(p2);
          rotate(p3);
          rotate(p4);
        }
        g2d.drawLine(p1.x, p1.y, p2.x, p2.y);
        g2d.drawLine(p2.x, p2.y, p3.x, p3.y);
        g2d.drawLine(p3.x, p3.y, p4.x, p4.y);
        if (dotsVisible) {
          center.setLocation(round(x), round(y));
          rotateIfSideways(center);
          g2d.fillRect(center.x, center.y, 2, 2);
          center.setLocation(round(x + deltaX), round(y + deltaY / 2));
          rotateIfSideways(center);
          g2d.fillRect(center.x, center.y, 2, 2);
        }
        x1 += deltaX;
        x2 += deltaX;
        x3 += deltaX;
        x4 += deltaX;
        y1 += 0.5F * deltaY;
        y2 += 0.5F * deltaY;
        y3 += 0.5F * deltaY;
        y4 += 0.5F * deltaY;
        p1.setLocation(round(x1), round(y1));
        p2.setLocation(round(x2), round(y2));
        p3.setLocation(round(x3), round(y3));
        p4.setLocation(round(x4), round(y4));
        if (sideways) {
          rotate(p1);
          rotate(p2);
          rotate(p3);
          rotate(p4);
        }
        g2d.drawLine(p1.x, p1.y, p2.x, p2.y);
        g2d.drawLine(p2.x, p2.y, p3.x, p3.y);
        g2d.drawLine(p3.x, p3.y, p4.x, p4.y);
        if (x == xmin) {
          p1.setLocation(round(x - r), round(y));
          p2.setLocation(round(x - r / 2), round(y + deltaY / 2));
          if (sideways) {
            rotate(p1);
            rotate(p2);
          }
          g2d.drawLine(p1.x, p1.y, p2.x, p2.y);
        }
      }
    }
    g2d.setClip(oldClip);
  }

  public void setGridNumbering(GridNumbering numbering) {
    this.numbering = numbering;
  }


  @Override
  public GridNumbering getGridNumbering() {
    return numbering;
  }


  @Override
  public Point getOrigin() {
    return new Point(origin);
  }

  @Override
  public void setOrigin(Point p) {
    origin.x = p.x;
    origin.y = p.y;
  }

  public void editGrid() {
    gridEditor = new HexGridEditor(this);
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

  public static class HexGridEditor extends GridEditor {
    private static final long serialVersionUID = 1L;

    public HexGridEditor(EditableGrid grid) {
      super(grid);
    }

    /*
     * Calculate approximate grid metrics based on the three adjacent points
     * picked out by the user.
     */
    @Override
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

    protected Point reverse(Point p) {
      return new Point(p.y, p.x);
    }

    protected void check(boolean sideways, Point p1, Point p2, Point p3) {

      int r = abs(p1.x - p2.x);
      int width = r * 3 / 2;
      if (width < 1) {
        reportShapeError();
        return;
      }
      int height = abs(p3.y - p2.y) * 2;

      int Xoff = min(p1.x, p2.x) % width + r/2;
      int col = min(p1.x, p2.x) / width;
      int Yoff = min(p1.y, p2.y) % height - (col % 2 == 1 ? 0 : height/2);
      if (Yoff < 0) Yoff += height;

      setMetrics(width, height, Xoff, Yoff, sideways);
    }

    protected void checkEnd(boolean sideways, Point p1, Point p2, Point p3) {
      if (abs((p1.x + p2.x) / 2 - p3.x) > ERROR_MARGIN) {
        reportShapeError();
        return;
      }

      int r = abs(p3.y - p1.y) * 2;
      int width = r * 3 / 2;
      int height = abs(p3.x - p2.x) * 2;

      int xOrigin = p1.y - (p3.y < p1.y ? 0 : r);
      int Xoff = xOrigin % width + r/2;
      int col = xOrigin / width;
      int Yoff = min(p1.x, p2.x) % height - (col % 2 == 1 ? 0 : height/2);

      setMetrics(width, height, Xoff, Yoff, sideways);
    }

    protected void setMetrics(int width, int height, int xoff, int yoff, boolean b) {

      grid.setDx(width);
      grid.setDy(height);
      grid.setOrigin(new Point(xoff, yoff));
      grid.setSideways(b);

    }

  }

  public int getSnapScale() {
    return snapScale;
  }

  public void setSnapScale(int snapScale) {
    this.snapScale = snapScale;
  }
}
