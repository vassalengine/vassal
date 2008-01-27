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
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import javax.swing.Icon;

import VASSAL.tools.ErrorDialog;

/**
 * An implementation of {@link Icon} using an {@link ImageOp} as a source.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class OpIcon implements Icon {
  protected ImageOp sop;

  /**
   * Creates an uninitialized icon.
   */
  public OpIcon() { }

  /**
   * Creates an <code>OpIcon</code> using a given <code>ImageOp</code> as
   * its image source. 
   *
   * @param op the <code>ImageOp</code> to be used by this <code>OpIcon</code>
   */ 
  public OpIcon(ImageOp op) {
    sop = op;
  }

  /** 
   * {@inheritDoc}
   *
   * <p>The given <code>ImageOp</code> is called asynchronously when painting,
   * so as not to block the Event Dispatch Thread.</p>
   *
   * @param c {@inheritDoc}
   * @param g {@inheritDoc}
   * @param x {@inheritDoc}
   * @param y {@inheritDoc}
   */
  public void paintIcon(Component c, Graphics g, int x, int y) {
    if (sop != null) {
      try {
        g.drawImage(sop.getImage(new Repainter(c, g.getClipBounds())), x, y, c);
      }
      catch (CancellationException e) {
        e.printStackTrace();
        ErrorDialog.raise(e, e.getMessage());
      }
      catch (InterruptedException e) {
        e.printStackTrace();
        ErrorDialog.raise(e, e.getMessage());
      }
      catch (ExecutionException e) {
        e.printStackTrace();
        ErrorDialog.raise(e, e.getMessage());
      }
    }
  }

  /** {@inheritDoc} */
  public int getIconWidth() {
    return sop == null ? -1 : sop.getWidth();
  }

  /** {@inheritDoc} */
  public int getIconHeight() {
    return sop == null ? -1 : sop.getHeight();
  }

  /**
   * Returns the <code>ImageOp</code> which produces this icon's
   * <code>Image</code>.
   *
   * @return the <code>ImageOp</code> for this <code>OpIcon</code>
   */
  public ImageOp getOp() {
    return sop;
  }

  /**
   * Sets the <code>ImageOp</code> which produces this icon's
   * <code>Image</code>.
   * 
   * @param op the <code>ImageOp</code>
   */
  public void setOp(ImageOp op) {
    sop = op;
  }
}
