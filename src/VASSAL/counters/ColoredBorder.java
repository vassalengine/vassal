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
package VASSAL.counters;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.List;

public class ColoredBorder implements Highlighter {
  protected Color c;
  protected int thickness;

  // Additional Highlighters
  protected List<Highlighter> highlighters = new ArrayList<Highlighter>();

  public ColoredBorder() {
    this(Color.black, 3);
  }

  public ColoredBorder(Color c, int thickness) {
    this.c = c;
    this.thickness = thickness;
  }

  public void addHighlighter(Highlighter h) {
    highlighters.add(h);
  }

  public void removeHighlighter(Highlighter h) {
    highlighters.remove(h);
  }

  public void draw(GamePiece p, Graphics g, int x, int y,
                   Component obs, double zoom) {
    if (thickness > 0) {
      if (c != null) {
        // Find the border by outsetting the bounding box, and then scaling
        // the shape to fill the outset.
        final Shape s = p.getShape();
        final Rectangle br = s.getBounds();

        // Don't bother if the shape is empty.
        if (!br.isEmpty()) {
          final double xzoom = (br.getWidth()+1) / br.getWidth();
          final double yzoom = (br.getHeight()+1) / br.getHeight();
          final AffineTransform t = AffineTransform.getTranslateInstance(x,y);
          t.scale(xzoom*zoom, yzoom*zoom);

          final Graphics2D g2d = (Graphics2D) g;
          final Stroke str = g2d.getStroke();
          g2d.setStroke(
            new BasicStroke(Math.max(1, Math.round(zoom*thickness))));
          g2d.setColor(c);
          g2d.draw(t.createTransformedShape(s));
          g2d.setStroke(str);
        }
      }
      else {
        highlightSelectionBounds(p, g, x, y, obs, zoom);
      }
    }

    // Draw any additional highlighters
    for (Highlighter h : highlighters) {
      h.draw(p, g, x, y, obs, zoom);
    }
  }

  protected void highlightSelectionBounds(GamePiece p, Graphics g, int x, int y, Component obs, double zoom) {
    Rectangle r = p.getShape().getBounds();
    g.setColor(c);
    for (int i = 1; i < thickness; ++i)
      g.drawRect(x + (int) (zoom * r.x) - i,
                 y + (int) (zoom * r.y) - i,
                 (int) (zoom * r.width) + 2 * i - 1,
                 (int) (zoom * r.height) + 2 * i - 1);
  }

  public Rectangle boundingBox(GamePiece p) {
    final Rectangle r = p.getShape().getBounds();
    r.translate(-thickness, -thickness);
    r.setSize(r.width + 2 * thickness, r.height + 2 * thickness);

    for (Highlighter h : highlighters) r.add(h.boundingBox(p));
    return r;
  }

  public void setColor(Color c) {
    this.c = c;
  }

  public Color getColor() {
    return c;
  }

  public int getThickness() {
    return thickness;
  }

  public void setThickness(int thickness) {
    this.thickness = thickness;
  }
}
