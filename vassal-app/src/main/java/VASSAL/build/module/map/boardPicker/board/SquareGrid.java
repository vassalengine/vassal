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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.SquareGridNumbering;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.StringEnum;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;

import javax.swing.JButton;
import java.awt.Color;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.util.HashMap;
import java.util.Map;

import static java.lang.Math.abs;
import static java.lang.Math.floor;
import static java.lang.Math.max;
import static java.lang.Math.min;
import static java.lang.Math.round;

public class SquareGrid extends AbstractConfigurable implements GeometricGrid, GridEditor.EditableGrid {
  protected double dx = 48.0;
  protected double dy = 48.0;
  protected int snapScale = 0;
  protected Point origin = new Point(24, 24);
  protected boolean visible = false;
  protected boolean edgesLegal = false;
  protected boolean cornersLegal = false;
  protected boolean dotsVisible = false;
  protected Color color = Color.black;
  protected GridContainer container;
  protected Map<Integer, Area> shapeCache = new HashMap<>();
  protected SquareGridEditor gridEditor;
  protected String rangeOption = RANGE_METRIC;
  protected boolean snapTo = true;

  private GridNumbering gridNumbering;

  @Override
  public GridNumbering getGridNumbering() {
    return gridNumbering;
  }

  public void setGridNumbering(GridNumbering gridNumbering) {
    this.gridNumbering = gridNumbering;
  }

  @Override
  public double getDx() {
    return dx;
  }

  @Override
  public void setDx(double d) {
    dx = d;
  }

  @Override
  public double getDy() {
    return dy;
  }

  @Override
  public void setDy(double d) {
    dy = d;
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

  @Override
  public boolean isSideways() {
    return false;
  }

  @Override
  public void setSideways(boolean b) {

  }

  @Override
  public GridContainer getContainer() {
    return container;
  }

  public static final String DX = "dx"; //$NON-NLS-1$
  public static final String DY = "dy"; //$NON-NLS-1$
  public static final String X0 = "x0"; //$NON-NLS-1$
  public static final String Y0 = "y0"; //$NON-NLS-1$
  public static final String VISIBLE = "visible"; //$NON-NLS-1$
  public static final String CORNERS = "cornersLegal"; //$NON-NLS-1$
  public static final String EDGES = "edgesLegal"; //$NON-NLS-1$
  public static final String COLOR = "color"; //$NON-NLS-1$
  public static final String DOTS_VISIBLE = "dotsVisible"; //$NON-NLS-1$
  public static final String RANGE = "range"; //$NON-NLS-1$
  public static final String RANGE_MANHATTAN = "Manhattan"; //$NON-NLS-1$
  public static final String RANGE_METRIC = "Metric"; //$NON-NLS-1$
  public static final String SNAP_TO = "snapTo"; //$NON-NLS-1$

  public static class RangeOptions extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{RANGE_METRIC, RANGE_MANHATTAN};
    }
  }

  public static final String[] RANGE_OPTIONS = { RANGE_METRIC, RANGE_MANHATTAN };
  public static final String[] RANGE_KEYS    = { "Editor.Grid.euclidean", "Editor.Grid.manhattan"};

  public static class RangeConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new TranslatingStringEnumConfigurer(key, name, RANGE_OPTIONS, RANGE_KEYS);
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      X0,
      Y0,
      DX,
      DY,
      RANGE,
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
      Resources.getString("Editor.x_offset"), //$NON-NLS-1$
      Resources.getString("Editor.y_offset"), //$NON-NLS-1$
      Resources.getString("Editor.RectangleGrid.width"), //$NON-NLS-1$
      Resources.getString("Editor.RectangleGrid.height"), //$NON-NLS-1$
      Resources.getString("Editor.RectangleGrid.range_method"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.snap"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.edges"), //$NON-NLS-1$
      Resources.getString("Editor.RectangleGrid.corners"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.show_grid"), //$NON-NLS-1$
      Resources.getString("Editor.Grid.center_dots"), //$NON-NLS-1$
      Resources.getString(Resources.COLOR_LABEL),
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      Integer.class,
      Integer.class,
      Double.class,
      Double.class,
      RangeConfig.class,
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
      return () -> (visible || dotsVisible);
    }
    else if (EDGES.equals(name) || CORNERS.equals(name)) {
      return () -> snapTo;
    }
    else {
      return super.getAttributeVisibility(name);
    }
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
    return Resources.getString("Editor.RectangleGrid.component_type"); //$NON-NLS-1$
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
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("RectangularGrid.html"); //$NON-NLS-1$
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
    else if (RANGE.equals(key)) {
      return rangeOption;
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
    }
    else if (DX.equals(key)) {
      if (val instanceof String) {
        val = Double.valueOf((String) val);
      }
      dx = (Double) val;
    }
    else if (RANGE.equals(key)) {
      rangeOption = (String) val;
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
    shapeCache.clear();
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{SquareGridNumbering.class};
  }

  @Override
  public Point getLocation(String location) throws BadCoords {
    if (gridNumbering == null) {
      throw new BadCoords();
    }
    return gridNumbering.getLocation(location);
  }

  @Override
  public int range(Point p1, Point p2) {
    if (rangeOption.equals(RANGE_METRIC)) {
      return max(
        abs((int) floor((p2.x - p1.x) / dx + 0.5)),
        abs((int) floor((p2.y - p1.y) / dy + 0.5)));
    }
    else {
      return abs((int) floor((p2.x - p1.x) / dx + 0.5))
          + abs((int) floor((p2.y - p1.y) / dy + 0.5));
    }
  }

  @Override
  public Area getGridShape(Point center, int range) {
    Area shape = shapeCache.get(range);
    if (shape == null) {
      shape = getSingleSquareShape(0, 0);
      final double dx = getDx();
      final double dy = getDy();

      for (int x = -range; x < range + 1; x++) {
        final int x1 = (int) (x * dx);
//        int yRange = range - abs(x); /* This creates a diamond-shaped range.  Configuration option?  */
        final int yRange = range;
        for (int y = -yRange; y < yRange + 1; y++) {
          final int y1 = (int) (y * dy);
          shape.add(getSingleSquareShape(x1, y1));
        }
      }
      shapeCache.put(range, shape);
    }
    shape = new Area(AffineTransform.getTranslateInstance(center.x, center.y).createTransformedShape(shape));
    return shape;
  }

  /**
   * Return the Shape of a single grid square
   */
  public Area getSingleSquareShape(int centerX, int centerY) {
    final double dx = getDx();
    final double dy = getDy();
    final Rectangle rect = new Rectangle((int) (centerX - dx / 2), (int) (centerY - dy / 2), (int) dx, (int) dy);
    return new Area(rect);
  }

  @Override
  public Point snapTo(Point p, boolean force) {
    if (!snapTo && !force) {
      return p;
    }

    // nx,ny are the closest points to the half-grid
    // (0,0) is the center of the origin cell
    // (1,0) is the east edge of the origin cell
    // (1,1) is the lower-right corner of the origin cell

    final int offsetX = p.x - origin.x;
    int nx = (int) round(offsetX / (0.5 * dx));
    final int offsetY = p.y - origin.y;
    int ny = (int) round(offsetY / (0.5 * dy));

    Point snap = null;

    if (!cornersLegal || !edgesLegal) {
      if (cornersLegal) {
        if (ny % 2 == 0) {  // on a cell center
          nx = 2 * (int) round(offsetX / dx);
        }
        else { // on a corner
          nx = 1 + 2 * (int) round(offsetX / dx - 0.5);
        }
      }
      else if (edgesLegal) {
        if (ny % 2 == 0) {
          if (nx % 2 == 0) { // Cell center
            nx = 2 * (int) round(offsetX / dx);
          }
          // else Vertical edge - do nothing
        }
        else { // Horizontal edge
          nx = 2 * (int) round(offsetX / dx);
        }
      }
      else {
        nx = 2 * (int)round(offsetX / dx);
        ny = 2 * (int)round(offsetY / dy);
        if (snapScale > 0) {
          int deltaX = offsetX - (int)round(nx * dx / 2);
          deltaX = (int)round(deltaX / (0.5 * dx / snapScale));
          deltaX = max(deltaX, 1 - snapScale);
          deltaX = min(deltaX, snapScale - 1);
          deltaX = (int)round(deltaX * 0.5 * dx / snapScale);
          int deltaY = offsetY - (int)round(ny * dy / 2);
          deltaY = (int)round(deltaY / (0.5 * dy / snapScale));
          deltaY = max(deltaY, 1 - snapScale);
          deltaY = min(deltaY, snapScale - 1);
          deltaY = (int)round(deltaY * 0.5 * dy / snapScale);
          snap = new Point((int)round(nx * dx / 2 + deltaX), (int)round(ny * dy / 2 + deltaY));
          snap.translate(origin.x, origin.y);
        }
      }
    }
    if (snap == null) {
      snap = new Point(origin.x + (int)round(nx * dx / 2), origin.y + (int) round(ny * dy / 2));
    }
    return snap;
  }

  @Override
  public Point snapTo(Point p) {
    return snapTo(p, false);
  }

  @Override
  public boolean isLocationRestricted(Point p) {
    return snapTo;
  }

  @Override
  public String locationName(Point p) {
    return gridNumbering == null ? null : gridNumbering.locationName(p);
  }

  @Override
  public String localizedLocationName(Point p) {
    return gridNumbering == null ? null : gridNumbering.localizedLocationName(p);
  }

  @Override
  public boolean isVisible() {
    return visible || dotsVisible || (gridNumbering != null && gridNumbering.isVisible());
  }

  @Override
  public void setVisible(boolean b) {
    visible = true;
  }

  protected void reverse(Point p, Rectangle bounds) {
    p.x = bounds.x + bounds.width - (p.x - bounds.x);
    p.y = bounds.y + bounds.height - (p.y - bounds.y);
  }

  /** Draw the grid, if visible, and accompanying numbering, if set */
  @Override
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if (visible || dotsVisible) {
      forceDraw(g, bounds, visibleRect, scale, reversed);
    }
    if (gridNumbering != null) {
      gridNumbering.draw(g, bounds, visibleRect, scale, reversed);
    }
  }

  /** Draw the grid even if not marked visible */
  public void forceDraw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if (!bounds.intersects(visibleRect) || color == null) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final Rectangle region = bounds.intersection(visibleRect);

    final Shape oldClip = g2d.getClip();
    if (oldClip != null) {
      final Area clipArea = new Area(oldClip);
      clipArea.intersect(new Area(region));
      g2d.setClip(clipArea);
    }

    final double deltaX = scale * dx;
    final double deltaY = scale * dy;

    double xmin = reversed ? bounds.x + scale * origin.x + bounds.width - deltaX * round((bounds.x + scale * origin.x + bounds.width - region.x) / deltaX) + deltaX / 2
        : bounds.x + scale * origin.x + deltaX * round((region.x - bounds.x - scale * origin.x) / deltaX) + deltaX / 2;
    final double xmax = region.x + region.width;
    double ymin = reversed ? bounds.y + scale * origin.y + bounds.height - deltaY * round((bounds.y + scale * origin.y + bounds.height - region.y) / deltaY) + deltaY / 2
        : bounds.y + scale * origin.y + deltaY * round((region.y - bounds.y - scale * origin.y) / deltaY) + deltaY / 2;
    final double ymax = region.y + region.height;

    final Point p1 = new Point();
    final Point p2 = new Point();
    g2d.setColor(color);

    // Draw Grid?
    if (visible) {
      // x is the location of a vertical line
      for (double x = xmin; x < xmax; x += deltaX) {
        p1.move((int) round(x), region.y);
        p2.move((int) round(x), region.y + region.height);
        g2d.drawLine(p1.x, p1.y, p2.x, p2.y);
      }
      for (double y = ymin; y < ymax; y += deltaY) {
        g2d.drawLine(region.x, (int) round(y), region.x + region.width, (int) round(y));
      }
    }

    // Draw center dots?
    if (dotsVisible) {
      xmin = reversed ? bounds.x + scale * origin.x + bounds.width - deltaX * round((bounds.x + scale * origin.x + bounds.width - region.x) / deltaX)
          : bounds.x + scale * origin.x + deltaX * round((region.x - bounds.x - scale * origin.x) / deltaX);
      ymin = reversed ? bounds.y + scale * origin.y + bounds.height - deltaY * round((bounds.y + scale * origin.y + bounds.height - region.y) / deltaY)
          : bounds.y + scale * origin.y + deltaY * round((region.y - bounds.y - scale * origin.y) / deltaY);
      for (double x = xmin; x < xmax; x += deltaX) {
        for (double y = ymin; y < ymax; y += deltaY) {
          p1.move((int) round(x - 0.5), (int) round(y - 0.5));
          g2d.fillRect(p1.x, p1.y, 2, 2);
        }
      }
    }
    g2d.setClip(oldClip);
  }

  @Override
  public Configurer getConfigurer() {
    final boolean buttonExists = config != null;
    final Configurer c = super.getConfigurer();
    if (!buttonExists) {
      final JButton b = new JButton(Resources.getString("Editor.Grid.edit_grid")); //$NON-NLS-1$
      b.addActionListener(e -> editGrid());
      ((Container) c.getControls()).add(b);
    }
    return c;
  }

  public void editGrid() {
    gridEditor = new SquareGridEditor(this);
    gridEditor.setVisible(true);
    // Local variables may have been updated by GridEditor so refresh
    // configurers.
    final AutoConfigurer cfg = (AutoConfigurer) getConfigurer();
    cfg.getConfigurer(DX).setValue(String.valueOf(dx));
    cfg.getConfigurer(DY).setValue(String.valueOf(dy));
    cfg.getConfigurer(X0).setValue(String.valueOf(origin.x));
    cfg.getConfigurer(Y0).setValue(String.valueOf(origin.y));
  }

  public static class SquareGridEditor extends GridEditor {
    private static final long serialVersionUID = 1L;

    public SquareGridEditor(EditableGrid grid) {
      super(grid);
    }

    /*
     * Calculate Grid metrics based on three selected points
     */
    @Override
    public void calculate() {
      if ((isPerpendicular(hp1, hp2) && isPerpendicular(hp1, hp3) && !isPerpendicular(hp2, hp3)) ||
          (isPerpendicular(hp2, hp1) && isPerpendicular(hp2, hp3) && !isPerpendicular(hp1, hp3)) ||
          (isPerpendicular(hp3, hp1) && isPerpendicular(hp3, hp2) && !isPerpendicular(hp1, hp2))) {
        final int height = max(abs(hp1.y - hp2.y), abs(hp1.y - hp3.y));
        final int width = max(abs(hp1.x - hp2.x), abs(hp1.x - hp3.x));
        final int top = min(hp1.y, min(hp2.y, hp3.y));
        final int left = min(hp1.x, min(hp2.x, hp3.x));
        grid.setDx(width);
        grid.setDy(height);
        setNewOrigin(new Point(left + width / 2, top + height / 2));
      }
      else {
        reportShapeError();
      }

    }

  }

  public int getSnapScale() {
    return snapScale;
  }

  public void setSnapScale(int snapScale) {
    this.snapScale = snapScale;
  }
}
