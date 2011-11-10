/*
 * $Id$
 *
 * Portions Copyright (c) 2010-2011 by Pieter Geerkens
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
import java.awt.Dimension;
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
import java.awt.geom.Dimension2D;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeSupport;
import java.util.HashMap;
import java.util.Map;
import static java.lang.Math.*;

import javax.swing.JButton;

import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.GeometricGrid;
import VASSAL.build.module.map.boardPicker.board.GridEditor;
import VASSAL.build.module.map.boardPicker.board.GridShearer.ShearableGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.HexGridNumberingIrregular;
import VASSAL.build.module.map.boardPicker.board.mapgrid.HexGridNumberingX;
import VASSAL.configure.AbstractAttributeListConfigurable;
import VASSAL.configure.Attribute.*;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.VisibilityCondition;

/**
 * A HexGrid is a map grid composed of hexes.
 */
public abstract class AbstractUIHexGrid extends AbstractAttributeListConfigurable
      implements GeometricGrid, GridEditor.EditableGrid, ShearableGrid {

  protected static final double sqrt3_2 = sqrt(3D) / 2D;
  
  // Property storage
  Boolean sideways = false;
  int x0 = 0, y0 = 32;
  double dx; // the 'HexWidth'
  double dy; // the 'HexSize'
  Boolean edgesLegal = false;
  Boolean cornersLegal = false;
  Boolean visible = false;
  Boolean dotsVisible = false;
  Color color = Color.black;
  double m00=1D, m01=0D, m02=0D, m10=0D, m11=1D, m12=0D;
  @Deprecated
  int snapScale = 0;

   // Property names
    public static final String SIDEWAYS = "sideways";
    public static final String X0 = "x0";
    public static final String Y0 = "y0";
    public static final String DY = "dy";
    public static final String DX = "dx";
    public static final String EDGES = "edgesLegal";
    public static final String CORNERS = "cornersLegal";
    public static final String VISIBLE = "visible";
    public static final String DOTS_VISIBLE = "dotsVisible";
    public static final String COLOR = "color";
    public static final String M00="m00",M01="m01",M02="m02",M10="m10",M11="m11",M12="m12";
    
    private static final String[] M = {M00,M10, M01,M11, M02,M12}; 
  private        final double[] m = {m00,m10, m01,m11, m02,m12}; // order is that of the AffineTransform constructor

    @Deprecated
     public static final String SNAP_SCALE = "snapScale";

  protected AffineTransform shear = AffineTransform.getTranslateInstance(0, 0);
  protected AffineTransform unshear =  AffineTransform.getTranslateInstance(0, 0);

    protected GridContainer container;
    protected GridNumbering numbering;
    protected Map<Integer,Area> shapeCache = new HashMap<Integer,Area>();
    protected HexGridEditor gridEditor;
    protected GridShearer gridShearer;

   /** A transform to reflect coordinates in line x=y, for sideways grids. 
    * This transform is its own inverse!
    */
   private final AffineTransform sidewaysTransform = new AffineTransform(0,1, 1,0, 0,0);

  public AbstractUIHexGrid() {
    super(16);
    addAttribute(SIDEWAYS,"Sideways (hexrows go horizontal)?  ");
    addAttribute(X0, "X offset:  ");
    addAttribute(Y0, "Y offset:  ");
    addAttribute(DY, "Hex Height:  ");
    addAttribute(DX, "Hex Width:  ");    
    addAttribute(EDGES,"Edges are legal locations?  ");
    addAttribute(CORNERS,"Vertices are legal locations?  ");
    addAttribute(VISIBLE,"Show grid?  ");
    addAttribute(DOTS_VISIBLE,"Draw center dots?  ");
    addAttribute(new ColorAttribute(COLOR, "Color:  ", 
      new VisibilityCondition() {
          @Override public boolean shouldBeVisible() { return visible; } }){
    });

    addAttribute(M00,M00,AbstractAttribute.invisible);
    addAttribute(M01,M01,AbstractAttribute.invisible);
    addAttribute(M02,M02,AbstractAttribute.invisible);
    addAttribute(M10,M10,AbstractAttribute.invisible);
    addAttribute(M11,M11,AbstractAttribute.invisible);
    addAttribute(M12,M12,AbstractAttribute.invisible);
    
    addAttribute(SNAP_SCALE, "Snap Scale: ", AbstractAttribute.invisible);

    if (changeSupport == null) { changeSupport = new PropertyChangeSupport(this); }
     changeSupport.firePropertyChange(null,null,null);
  }

  public static String getConfigureTypeName() { return "Hex Grid"; }
  
  /* ---------------------------------------------------------
  * Buildable implementation
  /* --------------------------------------------------------- */
  @Override
  public void addTo(Buildable b) {
     container = (GridContainer) b;
     container.setGrid(this);
  }
  @Override
  public void removeFrom(Buildable b) {
    ((GridContainer) b).removeGrid(this);
     changeSupport.firePropertyChange(null,null,null);
  }
  
  /* ---------------------------------------------------------
  * Configurable implementation
  /* --------------------------------------------------------- */
    @Override
    public void add(Buildable b) { super.add(b); }
  @Override
  public Configurer getConfigurer() {
    boolean buttonExists = config != null;
    
    AutoConfigurer c = (AutoConfigurer) super.getConfigurer();
    final Configurer dxConfig = c.getConfigurer(DX);
    c.getConfigurer(DY).addPropertyChangeListener(new java.beans.PropertyChangeListener() {
      @Override
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
        @Override
        public void actionPerformed(ActionEvent e) { editGrid(); }
      });
      ((Container) c.getControls()).add(b);

      JButton buttonGridShearer = new JButton("Shear Grid");
      buttonGridShearer.addActionListener(new ActionListener(){
        @Override 
        public void actionPerformed(ActionEvent e) { shearGrid(); }
      });
      ((Container) c.getControls()).add(buttonGridShearer);
    }
     return config;
  }
  @Override
  public String getConfigureName() {
    return null; 
  }
  @Override
  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HexGrid.htm");
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{
      HexGridNumberingX.class, HexGridNumberingIrregular.class
    };
  }

  /* ---------------------------------------------------------
  * MapGrid implementation
  /* --------------------------------------------------------- */
  @Override
  public String locationName(Point p) {
    return numbering == null ? null : numbering.locationName((Point)unshear.transform(p,new Point()));
  }
  
  @Override
  public String localizedLocationName(Point p) {
    return numbering == null ? null : numbering.localizedLocationName((Point)unshear.transform(p,new Point()));
  }

  @Override
  public Point getLocation(String location) throws BadCoords {
    if (numbering == null)
      throw new BadCoords();
    else
      return (Point) shear.transform(numbering.getLocation(location),new Point());
  }

  @Override
  public Point snapTo(Point p) {
    Point p0 = (Point) unshear.transform(p, new Point());
    Point p1;
    if (edgesLegal && cornersLegal) {
      Point edge   = snapToHexSide(p0);
      Point vertex = snapToHexVertex(p0);

      if( p0.distanceSq(edge) < p0.distanceSq(vertex) ) {
        p1 = edge;
      }
      else {
        p1 = vertex;
      }
    }
    else if (edgesLegal) {
      p1 = snapToHexSide(p0);
    }
    else if (cornersLegal) {
      p1 = snapToHexVertex(p0);
    }
    else {
      p1 = snapToHex(p0);
    }
    return (Point) shear.transform(p1,p1);
  }
  
  @Override
  public boolean isLocationRestricted(Point p) {
    return true;
  }

  /** @return the nearest hex-center  */
  protected abstract  Point snapToHex(Point p);

  /** @return the nearest hex-center or hex-side  */
  protected abstract  Point snapToHexSide(Point p);

  /** @return the nearest hex-center or hex-vertex  */
  protected abstract Point snapToHexVertex(Point p);

  @Override
  abstract public int range(Point p1, Point p2);

  /* ---------------------------------------------------------
  * GeometricGrid implementation
  /* --------------------------------------------------------- */
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
  
  /* ---------------------------------------------------------
  * Own implementation
  /* --------------------------------------------------------- */
  // TODO this should be private 
  public void rotateIfSideways(Point p) {  if (sideways) { rotate(p); } }

  private void rotate(Point p) {
    int swap = p.x;
    p.x = p.y;
    p.y = swap;
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

  protected Rectangle transformRectangle(Rectangle rIn, AffineTransform t) {
    final Point[] ap = {
      new Point(rIn.x, rIn.y),          new Point(rIn.x + rIn.width, rIn.y),
      new Point(rIn.x, rIn.y + rIn.height),  new Point(rIn.x + rIn.width, rIn.y + rIn.height)
      };
    t.transform(ap,0,ap,0,4);
    Point loc = new Point( (ap[0].x < ap[2].x) ? ap[0].x : ap[2].x,  
                    (ap[0].y < ap[1].y) ? ap[0].y : ap[1].y);
      Dimension dim = new Dimension( ( (ap[3].x > ap[1].x) ? ap[3].x : ap[1].x ) - loc.x,  
                           ( (ap[3].y > ap[2].y) ? ap[3].y : ap[2].y ) - loc.y);
      return new Rectangle(loc,  dim);
  }
  
    /** Draw the grid, if visible, and the accompanying numbering */
  @Override
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double zoom, boolean reversed) {
    if (visible) {
      forceDraw(g, bounds, visibleRect, zoom, reversed);
    }
    Graphics2D g2d = (Graphics2D) g;
    AffineTransform transform = g2d.getTransform();
    g2d.transform(shear);

    if (numbering != null) {
      numbering.draw(g, bounds, visibleRect, zoom, reversed);
    }
    g2d.setTransform(transform);
  }

  public void forceDraw(Graphics g, Rectangle bounds, Rectangle visibleRect, double zoom, boolean reversed) {
    Graphics2D g2d = (Graphics2D) g;
    AffineTransform transform = g2d.getTransform();
    g2d.transform(shear);
    
    forceDrawX(g2d, bounds, transformRectangle(visibleRect,unshear), zoom, reversed);
    drawCatchBuckets(g, bounds, visibleRect, zoom, reversed);
    
    g2d.setTransform(transform);
  }

  protected abstract void drawCatchBuckets(Graphics g, Rectangle bounds, 
      Rectangle visibleRect, double zoom, boolean reversed);

  /** Draw the grid even if set to be not visible */
  private void forceDrawX(Graphics g, Rectangle bounds, Rectangle visibleRect, double zoom, boolean reversed) {
    Point origin = new Point(x0,y0);
    if (!bounds.intersects(visibleRect)) {   return;   }
   
    Graphics2D g2d = (Graphics2D) g;
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);
    g2d.setColor(color == null ? Color.black : color);

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

    final float deltaX = (float) (dx * zoom);
    final float deltaY = (float) (dy * zoom);

    float xmin = bounds.x + (float)zoom*origin.x + 2*deltaX*(float)floor((region.x - zoom*origin.x - bounds.x)/(2*deltaX));
    float xmax = region.x + region.width + 2 * deltaX;
    float ymin = bounds.y + (float)zoom*origin.y + deltaY*(float)floor((region.y - zoom*origin.y - bounds.y)/deltaY);
    float ymax = region.y + region.height + deltaY;
    if (reversed) {
      xmin += bounds.width  - 2*deltaX*(float)floor(bounds.width/(2*deltaX)); 
      ymin += bounds.height -   deltaY*(float)floor(bounds.height/deltaY);
    }
    
    if (sideways) g2d.transform(sidewaysTransform);
    
    Dimension2D size = new Dimension((int)(2.F / 3.F * deltaX), (int)(-deltaY));
    Point2D center = new Point2D.Double();

    // x,y is the center of a hex
    for (float x = xmin - 2*deltaX; x < xmax; x += 2*deltaX) {
      for (float y = ymin; y < ymax; y += deltaY) {
        center.setLocation(x, y);
        drawCellTop(g2d, center, size);
        
        center.setLocation(x + deltaX, y + 0.5F * deltaY);
        drawCellTop(g2d, center, size);
      }
    }

    if (sideways) g2d.transform(sidewaysTransform);
    
    g2d.setClip(oldClip);
  }

  /** Utility class for {@link forceDrawX} that draws the upper-most three 
   * edges of a hex, and optionally it's center-dot.
   * @param g2d the Graphics2D context in which to draw
   * @param center of the hex to be drawn
   * @param size a Dimension2D object with the 
   */
  private void drawCellTop(Graphics2D g2d, Point2D center, Dimension2D size) {
    final int[] ax = new int[4];
    final int[] ay = new int[4];
    
    ax[0] = (int) (center.getX() - size.getWidth());     ay[0] = (int) center.getY();
    ax[1] = (int) (center.getX() - size.getWidth()/2D);  ay[1] = (int) (center.getY() + size.getHeight()/2D);
    ax[2] = (int) (center.getX() + size.getWidth()/2D);  ay[2] = ay[1];
    ax[3] = (int) (center.getX() + size.getWidth());      ay[3] = ay[0];
    g2d.drawPolyline(ax, ay, 4);

    if (dotsVisible) {
       g2d.fillOval((int)center.getX(), (int)center.getY(), 2, 2);
    }
  }
  
  public void setGridNumbering(GridNumbering numbering) {
    this.numbering = numbering;
  }

  public void shearGrid() {
    final AffineTransform saveShear = shear;
    
    gridShearer = new GridShearer(this);
    gridShearer.setVisible(true);

    AutoConfigurer cfg = (AutoConfigurer) getConfigurer();
    if (gridShearer.cancelled()) {
      saveShear.getMatrix(m);
    }
    else {
      shear.getMatrix(m);
    }
    for (int i = 0; i<6; i++) {
      cfg.getConfigurer(M[i]).setValue(m[i]);
    }
  }
  
  public void editGrid() {
    Point origin = new Point(x0,y0);
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
  
   public class HexGridEditor extends GridEditor {
     private static final long serialVersionUID = 1L; 

     public HexGridEditor(EditableGrid grid) {
       super(grid);
     }

     /**
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
    
     /**
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

     protected void check (@SuppressWarnings("hiding") boolean sideways, Point p1, Point p2, Point p3) {
      
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
    
     protected void checkEnd (@SuppressWarnings("hiding") boolean sideways, Point p1, Point p2, Point p3) {
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
         changeSupport.firePropertyChange(null,null,null);
     }
  }
/*
  @Deprecated
   public int getSnapScale() {
      return snapScale;
   }

  @Deprecated
   public void setSnapScale(int snapScale) {
      this.snapScale = snapScale;
     changeSupport.firePropertyChange(SNAP_SCALE,null,this.snapScale);
   }
*/

  @Override
  public AffineTransform getTransform() { return shear; }
  @Override
  public AffineTransform getInverse() { return unshear; }
  @Override
  public void setTransform(AffineTransform transform) throws NoninvertibleTransformException { 
    shear.setTransform(transform); 
    unshear.setTransform(transform);
    unshear.invert(); 
  }
  @Override
  public void clearTransform() { shear.setToIdentity(); unshear.setToIdentity(); }

  // EditableGrid implementation
  @Override
  public double getDx() { return dx; }

  @Override
  public double getDy() { return dy; }

  @Override
  public Point getOrigin() { return new Point(x0,y0); }

  @Override
  public boolean isSideways() {  return sideways; }
    
  @Override
  public void setDx(double d) {
    dx = d;
    changeSupport.firePropertyChange(DX, null, null);
  }

  @Override
  public void setDy(double d) {
    dy = d; // DO NOT call setHexSize() so that dx is not reset
    changeSupport.firePropertyChange(DY, null, dy);
  }

  @Override
  public void setOrigin(Point p) {
    x0 = p.x; y0 = p.y;
    changeSupport.firePropertyChange(null, null, null);
  }

  @Override
  public void setSideways(boolean b) { 
    sideways = b;
    changeSupport.firePropertyChange(SIDEWAYS, null, sideways);
   }

  @Override
  public GridContainer getContainer() { return container; }

  @Override
  public GridNumbering getGridNumbering() {
    return numbering;
  }

  @Override
  public boolean isVisible() { return visible; }

  @Override
  public void setVisible(boolean legal) {
    visible = legal;
    changeSupport.firePropertyChange(VISIBLE, null, visible);
  }

  @Override
  public String getGridName() {
    return getConfigureTypeName();
  }
}
