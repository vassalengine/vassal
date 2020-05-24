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
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.AffineTransform;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import VASSAL.tools.ErrorDialog;
import VASSAL.tools.swing.SwingUtils;

/**
 * An implementation of {@link Icon} using an {@link ImageOp} as a source.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class OpIcon extends ImageIcon implements Icon {
  private static final long serialVersionUID = 1L;
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
  @Override
  public void paintIcon(Component c, Graphics g, int x, int y) {
    if (sop == null) return;

    final Repainter r = c == null ? null : new Repainter(c, g.getClipBounds());

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    final AffineTransform orig_t = g2d.getTransform();
    g2d.setTransform(SwingUtils.descaleTransform(orig_t));

    x *= os_scale;
    y *= os_scale;

    try {
      g.drawImage(Op.scale(sop, os_scale).getImage(r), x, y, c);
    }
    catch (CancellationException | InterruptedException e) {
      ErrorDialog.bug(e);
    }
    catch (ExecutionException e) {
      if (!Op.handleException(e)) ErrorDialog.bug(e);
    }

    g2d.setTransform(orig_t);
  }

  /** {@inheritDoc} */
  @Override
  public Image getImage() {
    return sop == null ? null : sop.getImage();
  }

  /**
   * This method does nothing. It is overridden to prevent the
   * image from being set this way.
   */
  @Override
  public void setImage(Image img) {}

  /** {@inheritDoc} */
  @Override
  public int getIconWidth() {
    return sop == null ? -1 : sop.getWidth();
  }

  /** {@inheritDoc} */
  @Override
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
