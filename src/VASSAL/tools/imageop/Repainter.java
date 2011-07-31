/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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

package VASSAL.tools.imageop;

import java.awt.Component;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;

import VASSAL.tools.ErrorDialog;
import VASSAL.tools.opcache.Op;

/**
 * An <code>ImageOpObserver</code> which repaints {@link Component}s.
 * This class stores a reference to a <code>Component</code> and a
 * rectangle of that component which will be repainted when
 * {@link #imageOpChange} is called and <code>success</code> is true.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class Repainter implements ImageOpObserver {
  protected final Component c;
  protected final int x;
  protected final int y;
  protected final int w;
  protected final int h;

  /**
   * Creates a <code>Repainter</code> for the specified component and
   * rectangle. <code>Repainter</code>s will usually be constructed
   * during paint operations on the Event Dispatch Thread, and so will be
   * created in enormous quantities. In order to minimize object creation,
   * always use this constructor rather
   * {@link #Repainter(Component c, Rectangle r)}, except in the case where
   * the <code>Rectangle</code> already exists.
   *
   * @param c the component to repaint
   * @param x the x coordinate of the upper-left corner of the
   *          rectangle to repaint
   * @param y the y coordinate of the upper-left corner of the
   *          rectangle to repaint
   * @param w the width of the rectangle to repaint
   * @param h the height of the rectangle to repaint
   *
   * @throws IllegalArgumentException if <code>c == null</code>
   */
  public Repainter(Component c, int x, int y, int w, int h) {
    if (c == null) throw new IllegalArgumentException();

    this.c = c;
    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
  }

  /**
   * Creates a <code>Repainter</code> for the specified component and area.
   *
   * @param c the component to be repainted
   * @param r the area to be repainted
   *
   * @throws IllegalArgumentException if <code>c == null</code>
   */
  public Repainter(Component c, Rectangle r) {
    if (c == null) throw new IllegalArgumentException();
    this.c = c;

    if (r == null) r = c.getBounds();

    this.x = r.x;
    this.y = r.y;
    this.w = r.width;
    this.h = r.height;
  }

  /**
   * {@inheritDoc}
   *
   * @param op {@inheritDoc}
   * @param success repaint the <code>Component</code> iff <code>true</code>
   */
  public void imageOpChange(ImageOp op, boolean success) {
    if (success) c.repaint(x, y, w, h);
  }

  public void succeeded(Op<BufferedImage> op, BufferedImage img) {
    c.repaint(x, y, w, h);
  }

  public void cancelled(Op<BufferedImage> op, CancellationException e) {
    ErrorDialog.bug(e);
  }

  public void interrupted(Op<BufferedImage> op, InterruptedException e) {
    ErrorDialog.bug(e);
  }

  public void failed(Op<BufferedImage> op, ExecutionException e) {
    if (!VASSAL.tools.imageop.Op.handleException(e)) {
      ErrorDialog.bug(e);
    }
  }
}
