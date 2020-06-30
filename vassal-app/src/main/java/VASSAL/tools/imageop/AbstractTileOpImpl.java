/*
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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 * The abstract base class for {@link ImageOp}s which are
 * a single tile. This class provides simpler implementations of the
 * tile-related methods than does {@link AbstractTiledOp}, as objects
 * instantiating this class' subclasses are all single tiles.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public abstract class AbstractTileOpImpl extends AbstractOpImpl {
  /**
   * {@inheritDoc}
   *
   * This implementation is equivalent to {@link #getSize}.
   */
  @Override
  public Dimension getTileSize() {
    return getSize();
  }

  /**
   * {@inheritDoc}
   *
   * This implementation is equivalent to {@link #getHeight}.
   */
  @Override
  public int getTileHeight() {
    return getHeight();
  }

  /**
   * {@inheritDoc}
   *
   * This implementation is equivalent to {@link #getWidth}.
   */
  @Override
  public int getTileWidth() {
    return getWidth();
  }

  /**
   * {@inheritDoc}
   *
   * @return <code>1</code>, always
   */
  @Override
  public int getNumXTiles() {
    return 1;
  }

  /**
   * {@inheritDoc}
   *
   * @return <code>1</code>, always
   */
  @Override
  public int getNumYTiles() {
    return 1;
  }

  /**
   * {@inheritDoc}
   *
   * @return <code>getImage(obs)</code>, because there is only one tile
   * @throws IndexOutOfBoundsException
   *    If <code>tileX != 0</code> or <code>tileY != 0</code>.
   */
  @Override
  public BufferedImage getTile(int tileX, int tileY, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException
  {
    // we are but a single humble tile
    if (tileX != 0 || tileY != 0)
      throw new IndexOutOfBoundsException();

    return getImage(obs);
  }

  /**
   * {@inheritDoc}
   *
   * @return <code>getFutureImage(obs)</code>, because there is only one tile
   * @throws IndexOutOfBoundsException
   *    If <code>tileX != 0</code> or <code>tileY != 0</code>.
   */
  @Override
  public Future<BufferedImage> getFutureTile(
    int tileX, int tileY, ImageOpObserver obs) throws ExecutionException
  {
    // we are but a single humble tile
    if (tileX != 0 || tileY != 0)
      throw new IndexOutOfBoundsException();

    return getFutureImage(obs);
  }

  @Override
  public ImageOp getTileOp(int tileX, int tileY) {
    // we are but a single humble tile
    if (tileX != 0 || tileY != 0)
      throw new IndexOutOfBoundsException();

    return this;
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalArgumentException if <code>rect == null</code>.
   */
  @Override
  public Point[] getTileIndices(Rectangle rect) {
    if (rect == null) throw new IllegalArgumentException();
    return rect.intersects(new Rectangle(size)) ?
      new Point[]{new Point(0,0)} : new Point[0];
  }
}
