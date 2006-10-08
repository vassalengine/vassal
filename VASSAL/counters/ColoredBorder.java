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

public class ColoredBorder implements Highlighter {
  private Color c;
  private int thickness;

  public ColoredBorder() {
    this(Color.black, 3);
  }

  public ColoredBorder(Color c, int thickness) {
    this.c = c;
    this.thickness = thickness;
  }

  public void draw(GamePiece p, Graphics g, int x, int y,
                   Component obs, double zoom) {
    if (c == null || thickness <= 0) {
      return;
    }
    if (g instanceof Graphics2D) {
      Graphics2D g2d = (Graphics2D) g;
      Stroke str = g2d.getStroke();
      g2d.setStroke(new BasicStroke(Math.max(1,Math.round(zoom*thickness))));
      g2d.setColor(c);

      // Find the border by outsetting the bounding box, and then scaling
      // the shape to fill the outset.
      Shape s = p.getShape();
      Rectangle br = s.getBounds();
      double xzoom = (br.getWidth()+1)/br.getWidth();
      double yzoom = (br.getHeight()+1)/br.getHeight();
      AffineTransform t = AffineTransform.getTranslateInstance(x,y);
      t.scale(xzoom*zoom,yzoom*zoom);

      g2d.draw(t.createTransformedShape(s));
      g2d.setStroke(str);
    }
    else {
      highlightSelectionBounds(p, g, x, y, obs, zoom);
    }
  }

  private void highlightSelectionBounds(GamePiece p, Graphics g, int x, int y, Component obs, double zoom) {
    Rectangle r = p.getShape().getBounds();
    g.setColor(c);
    for (int i = 1; i < thickness; ++i)
      g.drawRect(x + (int) (zoom * r.x) - i,
                 y + (int) (zoom * r.y) - i,
                 (int) (zoom * r.width) + 2 * i - 1,
                 (int) (zoom * r.height) + 2 * i - 1);
  }

  public java.awt.Rectangle boundingBox(GamePiece p) {
    Rectangle r = p.getShape().getBounds();
    r.translate(-thickness, -thickness);
    r.setSize(r.width + 2 * thickness, r.height + 2 * thickness);
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
