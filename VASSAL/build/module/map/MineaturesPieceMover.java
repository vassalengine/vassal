package VASSAL.build.module.map;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.ColoredBorder;
import VASSAL.counters.DragBuffer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Transparent;

/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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

/**
 * Moves pieces on a map with an interface appropriate for mineatures-type games.
 * Only one piece may be moved at a time, with a transparent outline of the
 * piece being dragged and a line connecting it to its original location.
 * An maximum distance for moving pieces may be specified.
 */
public class MineaturesPieceMover extends PieceMover implements MouseMotionListener, Drawable {
  protected GamePiece dragging;
  protected Transparent trans;
  /** The piece's original location in component coordinates */
  protected Point anchor;
  /** The piece's final destination, in component coordinates */
  protected Point arrow;
  protected Color lineColor = Color.black;

  public void addTo(Buildable b) {
    super.addTo(b);
    map.setDragGestureListener(null);
    ((Map) b).addDrawComponent(this);
    ((Map) b).getView().addMouseMotionListener(this);
  }

  public void mousePressed(MouseEvent e) {
    super.mousePressed(e);
    PieceIterator pi = DragBuffer.getBuffer().getIterator();
    if (pi.hasMoreElements()) {
      dragging = pi.nextPiece();
      anchor = map.componentCoordinates(dragging.getPosition());
      trans = new Transparent(dragging);
      trans.setAlpha(0.5);
      if (map.getHighlighter() instanceof ColoredBorder) {
        lineColor = ((ColoredBorder)map.getHighlighter()).getColor();
      }
      arrow = null;
    }
    else {
      clear();
    }
  }

  protected boolean isMultipleSelectionEvent(MouseEvent e) {
    return false;
  }

  protected void clear() {
    anchor = null;
    arrow = null;
    trans = null;
  }

  public void draw(Graphics g, Map map) {
    if (anchor != null
        && arrow != null) {
      g.setColor(lineColor);
      Point p2 = getDragDestination(map.mapCoordinates(anchor), map.mapCoordinates(arrow));
      if (p2 != null) {
        p2 = map.componentCoordinates(p2);
        g.drawLine(anchor.x, anchor.y, p2.x, p2.y);
        trans.draw(g, p2.x, p2.y, map.getView(), map.getZoom());
      }
    }
  }

  public boolean drawAboveCounters() {
    return true;
  }

  /**
   * A Mouse release event will be translated to this location (in map coordinates).
   * This allows restrictions, such as distance limitations, to be put on where a piece may move.
   * @return null if the move is invalid
   */
  protected Point getDragDestination(Point src, Point dest) {
    Point p = dest;
    if (dest != null
        && src != null) {
      double dist = Math.sqrt(Math.pow(dest.x - src.x, 2.0) + Math.pow((dest.y - src.y), 2.0));
      int maxDrag = getMaxDragDistance(dragging);
      if (dist > maxDrag) {
        p = new Point(src.x + (int) Math.round(maxDrag * (dest.x - src.x) / dist),
                      src.y + (int) Math.round(maxDrag * (dest.y - src.y) / dist));
      }
    }
    return p;
  }

  public void mouseDragged(MouseEvent e) {
    arrow = e.getPoint();
    map.repaint();
  }

  public void mouseReleased(MouseEvent e) {
    if (anchor != null) {
      Point p = getDragDestination(map.mapCoordinates(anchor), e.getPoint());
      e.translatePoint(p.x - e.getX(), p.y - e.getY());
    }
    super.mouseReleased(e);
    clear();
  }

  /**
   * The maximum distance that a piece may be dragged, in map-coordinate pixels
   * @param piece
   * @return
   */
  protected int getMaxDragDistance(GamePiece piece) {
    return Integer.MAX_VALUE;
  }

  public void mouseMoved(MouseEvent e) {
  }

}
