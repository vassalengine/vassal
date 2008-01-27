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

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;

/**
 * An {@link ImageOp} which uses a {@link GamePiece} as its source.
 * 
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class GamePieceOp extends AbstractTileOp {
  /** The image source for this <code>ImageOp</code>. */
  protected GamePiece piece;

  private String lastState = null;
  private final int hash;

  /**
   * Constructs an <code>ImageOp</code> which will produce an image
   * from the given <code>GamePiece</code>.
   *
   * @param gp the source
   * @throws IllegalArgumentException if <code>gp == null</code>.
   */
  public GamePieceOp(GamePiece gp) {
    if (gp == null) throw new IllegalArgumentException();
    piece = gp;
    hash = piece.hashCode();
  }

  /**
   * {@inheritDoc}
   *
   * <p>This method is overriden to recalculate the <code>Image</code>
   * whenever the source <code>GamePiece</code> changes state.
   *
   * @param obs {@inheritDoc}
   * @return {@inheritDoc}
   */ 
  @Override
  public Image getImage(ImageOpObserver obs) 
    throws CancellationException, InterruptedException, ExecutionException {
// FIXME: does this work?
    if (isChanged()) {
      Future<Image> fim = cache.get(this);
      if (fim != null) cache.remove(this, fim);
    }
    return super.getImage(obs);
  }

  /**
   * {@inheritDoc}
   *
   * <p>This method is overriden to recalculate the <code>Image</code>
   * whenever the source <code>GamePiece</code> changes state.
   *
   * @param obs {@inheritDoc}
   * @return {@inheritDoc}
   */ 
  @Override
  public Future<Image> getFutureImage(ImageOpObserver obs)
    throws ExecutionException {
// FIXME: does this work?
    if (isChanged()) {
      Future<Image> fim = cache.get(this);
      if (fim != null) cache.remove(this, fim);
    }
    return super.getFutureImage(obs);
  }

  /** {@inheritDoc} */
  protected Image apply() {
    lastState = String.valueOf(piece.getProperty(Properties.VISIBLE_STATE)); 

    final Rectangle b = piece.boundingBox();
    BufferedImage im = new BufferedImage(Math.max(b.width, 1),
                                         Math.max(b.height, 1),
                                         BufferedImage.TYPE_INT_ARGB);
    final Graphics2D g = im.createGraphics();
    piece.draw(g, -b.x, -b.y, null, 1.0);
    g.dispose();
    return im;
  }

  /** {@inheritDoc} */
  protected void fixSize() {
    size = piece.boundingBox().getSize();
    if (size.width < 1) size.width = 1;
    if (size.height < 1) size.height = 1;
  }

  /**
   * {@inheritDoc}
   *
   * This method is overriden to recalculate the size whenever the
   * source <code>GamePiece</code> changes state.
   */
  @Override
  public Dimension getSize() {
    if (size == null || isChanged()) fixSize();
    return new Dimension(size);
  }

  /**
   * {@inheritDoc}
   *
   * This method is overriden to recalculate the width whenever the
   * source <code>GamePiece</code> changes state.
   */
  @Override
  public int getWidth() {
    if (size == null || isChanged()) fixSize();
    return size.width;
  }

  /**
   * {@inheritDoc}
   *
   * This method is overriden to recalculate the height whenever the
   * source <code>GamePiece</code> changes state.
   */
  @Override
  public int getHeight() {
    if (size == null || isChanged()) fixSize();
    return size.height;
  }

  /**
   * Returns the source <code>GamePiece</code>.
   *
   * @return the source
   */
  public GamePiece getPiece() {
    return piece;
  }

  /**
   * Returns <code>true</code> iff the source <code>GamePiece</code>
   * has changed state.
   *
   * @return <code>true</code> iff the source has changed.
   */
  public boolean isChanged() {
    return !String.valueOf(
      piece.getProperty(Properties.VISIBLE_STATE)).equals(lastState);
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;
    return piece.equals(((GamePieceOp) o).piece);
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
