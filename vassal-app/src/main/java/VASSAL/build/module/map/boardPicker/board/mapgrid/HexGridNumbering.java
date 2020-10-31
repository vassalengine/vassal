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

import javax.swing.JComponent;
import javax.swing.JPanel;

import org.apache.commons.lang3.ArrayUtils;

import VASSAL.build.Buildable;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.i18n.Resources;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.image.LabelUtils;
import VASSAL.tools.swing.SwingUtils;

public class HexGridNumbering extends RegularGridNumbering {
  private HexGrid grid;
  private boolean stagger = true;

  @Override
  public void addTo(Buildable parent) {
    grid = (HexGrid) parent;
    grid.setGridNumbering(this);
  }

  public static final String STAGGER = "stagger"; //NON-NLS

  public HexGrid getGrid() {
    return grid;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.add(
      super.getAttributeDescriptions(),
      Resources.getString("Editor.HexGridNumbering.stagger")
    );
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.add(
      super.getAttributeNames(),
      STAGGER
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.add(
      super.getAttributeTypes(),
      Boolean.class
    );
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (STAGGER.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      stagger = (Boolean) value;
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (STAGGER.equals(key)) {
      return String.valueOf(stagger);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  /** Draw the numbering if visible */
  @Override
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if (visible) {
      forceDraw(g, bounds, visibleRect, scale, reversed);
    }
  }

  /** Draw the numbering, even if not visible */
  public void forceDraw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    final int size = (int) (scale * fontSize + 0.5);
    if (size < 5) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final AffineTransform oldT = g2d.getTransform();
    if (reversed) {
      final AffineTransform t = AffineTransform.getRotateInstance(
        Math.PI,
        bounds.x + 0.5 * bounds.width,
        bounds.y + 0.5 * bounds.height
      );
      g2d.transform(t);
      visibleRect = t.createTransformedShape(visibleRect).getBounds();
    }

    if (!bounds.intersects(visibleRect)) {
      return;
    }

    Rectangle region = bounds.intersection(visibleRect);

    final Shape oldClip = g.getClip();
    if (oldClip != null) {
      final Area clipArea = new Area(oldClip);
      clipArea.intersect(new Area(region));
      g.setClip(clipArea);
    }

    final double deltaX = scale * grid.getHexWidth();
    final double deltaY = scale * grid.getHexSize();

    if (grid.isSideways()) {
      bounds = new Rectangle(bounds.y, bounds.x, bounds.height, bounds.width);
      region = new Rectangle(region.y, region.x, region.height, region.width);
    }

    final int minCol = 2 * (int) Math.floor((region.x - bounds.x - scale * grid.getOrigin().x) / (2 * deltaX));
    final double xmin = bounds.x + scale * grid.getOrigin().x + deltaX * minCol;
    final double xmax = region.x + region.width + deltaX;
    final int minRow = (int) Math.floor((region.y - bounds.y - scale * grid.getOrigin().y) / deltaY);
    final double ymin = bounds.y + scale * grid.getOrigin().y + deltaY * minRow;
    final double ymax = region.y + region.height + deltaY;

    final Font f = new Font(Font.DIALOG, Font.PLAIN, size);
    int alignment = LabelUtils.TOP;
    int offset = -(int) Math.round(deltaY / 2);
    if (grid.isSideways() || rotateTextDegrees != 0) {
      alignment = LabelUtils.CENTER;
      offset = 0;
    }

    final Point p = new Point();
    final Point gridp = new Point();

    Point centerPoint = null;
    double radians = 0;
    if (rotateTextDegrees != 0) {
      radians = Math.toRadians(rotateTextDegrees);
      g2d.rotate(radians);
    }

    for (double x = xmin; x < xmax; x += 2 * deltaX) {
      for (double y = ymin; y < ymax; y += deltaY) {
        p.setLocation((int) Math.round(x), (int) Math.round(y) + offset);
        gridp.setLocation(p.x, p.y - offset);
        grid.rotateIfSideways(p);

        // Convert from map co-ordinates to board co-ordinates
        gridp.translate(-bounds.x, -bounds.y);
        grid.rotateIfSideways(gridp);
        gridp.x = (int) Math.round(gridp.x / scale);
        gridp.y = (int) Math.round(gridp.y / scale);

        centerPoint = offsetLabelCenter(p, scale);
        LabelUtils.drawLabel(
          g2d, getName(getRow(gridp), getColumn(gridp)),
          centerPoint.x, centerPoint.y,
          f, LabelUtils.CENTER, alignment, color, null, null
        );

        p.setLocation((int) Math.round(x + deltaX), (int) Math.round(y + deltaY / 2) + offset);
        gridp.setLocation(p.x, p.y - offset);
        grid.rotateIfSideways(p);

        // Convert from map co-ordinates to board co-ordinates
        gridp.translate(-bounds.x, -bounds.y);
        grid.rotateIfSideways(gridp);
        gridp.x = (int) Math.round(gridp.x / scale);
        gridp.y = (int) Math.round(gridp.y / scale);

        centerPoint = offsetLabelCenter(p, scale);
        LabelUtils.drawLabel(
          g2d, getName(getRow(gridp), getColumn(gridp)),
          centerPoint.x, centerPoint.y,
          f, LabelUtils.CENTER, alignment, color, null, null
        );
      }
    }

    if (rotateTextDegrees != 0) {
      g2d.rotate(-radians);
    }
    g.setClip(oldClip);
    g2d.setTransform(oldT);
  }

  @Override
  public Point getCenterPoint(int col, int row) {
    if (stagger) {
      if (grid.isSideways()) {
        if (col % 2 != 0) {
          if (hDescending)
            row++;
          else
            row--;
        }
      }
      else {
        if (col % 2 != 0) {
          if (vDescending)
            row++;
          else
            row--;
        }
      }
    }

    if (grid.isSideways()) {
      if (vDescending)
        col = getMaxRows() - col;
      if (hDescending)
        row = getMaxColumns() - row;
    }
    else {
      if (hDescending)
        col = getMaxColumns() - col;
      if (vDescending)
        row = getMaxRows() - row;
    }

    final Point p = new Point();

    p.x = (int) (col * grid.getHexWidth());
    p.x += grid.getOrigin().x;

    if (col % 2 == 0)
      p.y = (int) (row * grid.getHexSize());
    else
      p.y = (int) (row * grid.getHexSize() + grid.getHexSize() / 2);
    p.y += grid.getOrigin().y;

    grid.rotateIfSideways(p);
    return p;
  }

  @Override
  public int getColumn(Point p) {

    int x = getRawColumn(p);

    if (vDescending && grid.isSideways()) {
      x = (getMaxRows() - x);
    }
    if (hDescending && !grid.isSideways()) {
      x = (getMaxColumns() - x);
    }

    return x;
  }

  public int getRawColumn(Point p) {
    p = new Point(p);
    grid.rotateIfSideways(p);
    int x = p.x - grid.getOrigin().x;

    x = (int) Math.floor(x / grid.getHexWidth() + 0.5);
    return x;
  }

  @Override
  protected JComponent getGridVisualizer() {
    if (visualizer == null) {
      visualizer = new JPanel() {
        private static final long serialVersionUID = 1L;

        @Override
        public void paint(Graphics g) {
          final Graphics2D g2d = (Graphics2D) g;
          final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
          final AffineTransform orig_t = g2d.getTransform();
          g2d.setTransform(SwingUtils.descaleTransform(orig_t));

          final Rectangle bounds = new Rectangle(0, 0, getWidth(), getHeight());
          bounds.x *= os_scale;
          bounds.y *= os_scale;
          bounds.width *= os_scale;
          bounds.height *= os_scale;

          g.clearRect(0, 0, bounds.width, bounds.height);
          grid.forceDraw(g, bounds, bounds, os_scale, false);
          forceDraw(g, bounds, bounds, os_scale, false);

          g2d.setTransform(orig_t);
        }

        @Override
        public Dimension getPreferredSize() {
          return new Dimension(4 * (int) grid.getHexSize(), 4 * (int) grid.getHexWidth());
        }
      };
    }
    return visualizer;
  }

  @Override
  public int getRow(Point p) {

    int ny = getRawRow(p);

    if (vDescending && !grid.isSideways()) {
      ny = (getMaxRows() - ny);
    }
    if (hDescending && grid.isSideways()) {
      ny = (getMaxColumns() - ny);
    }

    if (stagger) {
      if (grid.isSideways()) {
        if (getRawColumn(p) % 2 != 0) {
          if (hDescending) {
            ny--;
          }
          else {
            ny++;
          }
        }
      }
      else {
        if (getRawColumn(p) % 2 != 0) {
          if (vDescending) {
            ny--;
          }
          else {
            ny++;
          }
        }
      }
    }
    return ny;
  }

  protected int getRawRow(Point p) {
    p = new Point(p);
    grid.rotateIfSideways(p);
    final Point origin = grid.getOrigin();
    final double dx = grid.getHexWidth();
    final double dy = grid.getHexSize();
    final int nx = (int) Math.round((p.x - origin.x) / dx);
    final int ny;
    if (nx % 2 == 0) {
      ny = (int) Math.round((p.y - origin.y) / dy);
    }
    else {
      ny = (int) Math.round((p.y - origin.y - dy / 2) / dy);
    }
    return ny;
  }

  @Override
  public void removeFrom(Buildable parent) {
    grid.setGridNumbering(null);
  }

  protected int getMaxRows() {
    return (int) Math.floor(grid.getContainer().getSize().height / grid.getHexWidth() + 0.5);
  }

  protected int getMaxColumns() {
    return (int) Math.floor(grid.getContainer().getSize().width / grid.getHexSize() + 0.5);
  }
}
