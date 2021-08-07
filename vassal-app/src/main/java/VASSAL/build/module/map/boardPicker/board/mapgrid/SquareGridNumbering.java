/*
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

import VASSAL.build.Buildable;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.tools.image.LabelUtils;
import VASSAL.tools.swing.SwingUtils;

public class SquareGridNumbering extends RegularGridNumbering {

  private SquareGrid grid;

  @Override
  public void addTo(Buildable parent) {
    grid = (SquareGrid) parent;
    grid.setGridNumbering(this);
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

          final double orig_dx = grid.getDx();
          final double orig_dy = grid.getDy();
          double new_dx = orig_dx;
          double new_dy = orig_dy;

          if (orig_dx > VISUALIZER_GRID_SIZE * 2 && orig_dx > orig_dy) {
            new_dx = VISUALIZER_GRID_SIZE * 2;
            new_dy = orig_dy * VISUALIZER_GRID_SIZE * 2 / orig_dx;
          }
          else if (orig_dy > VISUALIZER_GRID_SIZE * 2 && orig_dy > orig_dx) {
            new_dy = VISUALIZER_GRID_SIZE * 2;
            new_dx = orig_dx * VISUALIZER_GRID_SIZE * 2 / orig_dy;
          }

          grid.setDx(new_dx);
          grid.setDy(new_dy);

          final Rectangle bounds = new Rectangle(0, 0, getWidth(), getHeight());
          bounds.x *= os_scale;
          bounds.y *= os_scale;
          bounds.width *= os_scale;
          bounds.height *= os_scale;

          g.clearRect(0, 0, bounds.width, bounds.height);
          grid.forceDraw(g, bounds, bounds, os_scale, false);
          forceDraw(g, bounds, bounds, os_scale, false);

          g2d.setTransform(orig_t);
          grid.setDx(orig_dx);
          grid.setDy(orig_dy);
        }

        @Override
        public Dimension getPreferredSize() {
          return new Dimension(6 * (int) VISUALIZER_GRID_SIZE, 4 * (int) VISUALIZER_GRID_SIZE);
        }
      };
    }
    return visualizer;
  }

  /** Draw the numbering, if visible */
  @Override
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    if (visible) {
      forceDraw(g, bounds, visibleRect, scale, reversed);
    }
  }

  /** Draw the numbering, even if not visible */
  public void forceDraw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    final int size = (int) (scale * fontSize + 0.5);
    if (size < 5 || !bounds.intersects(visibleRect)) {
      return;
    }
    final Rectangle region = bounds.intersection(visibleRect);
    final Shape oldClip = g.getClip();
    if (oldClip != null) {
      final Area clipArea = new Area(oldClip);
      clipArea.intersect(new Area(region));
      g.setClip(clipArea);
    }

    final double deltaX = scale * grid.getDx();
    final double deltaY = scale * grid.getDy();

    Point centerPoint;
    final Graphics2D g2d = (Graphics2D) g;
    double radians = 0;
    if (rotateTextDegrees != 0) {
      radians = Math.toRadians(rotateTextDegrees);
      g2d.rotate(radians);
    }

    final int minCol = reversed ? (int) Math.ceil((bounds.x - scale * grid.getOrigin().x + bounds.width - region.x) / deltaX)
        : (int) Math.floor((region.x - bounds.x - scale * grid.getOrigin().x) / deltaX);
    final double xmin = reversed ? bounds.x - scale * grid.getOrigin().x + bounds.width - deltaX * minCol
        : bounds.x + scale * grid.getOrigin().x + deltaX * minCol;
    final double xmax = region.x + region.width + deltaX;
    final int minRow = reversed ? (int) Math.ceil((bounds.y - scale * grid.getOrigin().y + bounds.height - region.y) / deltaY)
        : (int) Math.floor((region.y - bounds.y - scale * grid.getOrigin().y) / deltaY);
    final double ymin = reversed ? bounds.y - scale * grid.getOrigin().y + bounds.height - deltaY * minRow
        : bounds.y + scale * grid.getOrigin().y + deltaY * minRow;
    final double ymax = region.y + region.height + deltaY;

    final Font f = new Font(Font.DIALOG, Font.PLAIN, size);
    int column = minCol;
    for (double x = xmin; x < xmax; x += deltaX, column += reversed ? -1 : 1) {
      int printRow, printColumn;
      int row = minRow;
      for (double y = ymin; y < ymax; y += deltaY, row += reversed ? -1 : 1) {
        printRow = row;
        printColumn = column;
        if (vDescending) {
          printRow = getMaxRows() - row;
        }
        if (hDescending) {
          printColumn = getMaxColumns() - column;
        }

        // When rotating text, keep basic label position as in center along edge
        final int newX;
        final int newY;
        switch (rotateTextDegrees) {
        case 90:
          newX = (int) (x + deltaX / 2);
          newY = (int) y;
          break;
        case 180:
          newX = (int) x;
          newY = (int) (y + deltaY / 2);
          break;
        case 270:
          newX = (int) (x - deltaX / 2);
          newY = (int) y;
          break;
        default :
          newX = (int) x;
          newY = (int) (y - deltaY / 2);
          break;
        }

        centerPoint = offsetLabelCenter(newX, newY, scale);
        LabelUtils.drawLabel(
          g, getName(printRow, printColumn),
          centerPoint.x, centerPoint.y,
          f, LabelUtils.CENTER, LabelUtils.TOP, color, null, null
        );
      }
    }
    if (rotateTextDegrees != 0) {
      g2d.rotate(-radians);
    }
    g.setClip(oldClip);
  }

  @Override
  public int getColumn(Point p) {
    final int col = (int) Math.floor((p.x - grid.getOrigin().x) / grid.getDx() + 0.5);
    if (hDescending) {
      return (getMaxColumns() - col);
    }
    else {
      return col;
    }
  }

  @Override
  public int getRow(Point p) {
    final int row = (int) ((p.y - grid.getOrigin().y) / grid.getDy() + 0.5);
    if (vDescending) {
      return (getMaxRows() - row);
    }
    else {
      return row;
    }
  }

  @Override
  public Point getCenterPoint(int col, int row) {
    if (vDescending)
      row = getMaxRows() - row;
    if (hDescending)
      col = getMaxColumns() - col;

    // TODO: invoke grid.snapTo
    return new Point((int) (col * grid.getDx() + grid.getOrigin().x),
        (int) (row * grid.getDy() + grid.getOrigin().y));
  }

  @Override
  public void removeFrom(Buildable parent) {
    grid.setGridNumbering(null);
  }

  protected int getMaxRows() {
    return (int) (grid.getContainer().getSize().height / grid.getDy() + 0.5);
  }

  protected int getMaxColumns() {
    return (int) (grid.getContainer().getSize().width / grid.getDx() + 0.5);
  }
}
