/*
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

import VASSAL.tools.ErrorDialog;
import VASSAL.tools.opcache.OpCache;

/**
 * An abstract representation of an operation which may be applied to an
 * {@link Image}. <code>ImageOp</code> is the base class for all such
 * operations. The results of all operations are memoized (using a
 * memory-sensitive cache), so retrieving results is both fast and
 * memory-efficient.
 *
 * <p><b>Warning:</b> For efficiency reasons, the methods {@link #getImage}
 * and {@link #getTile} do <em>not</em> return <code>Image</code>s
 * defensively, nor do the {@code Future<Image>}s returned by
 * {@link #getFutureImage} and {@link #getFutureTile}. That is, the
 * <code>Image</code>  returned is possibly the one retained internally by
 * the <code>ImageOp</code>. Therefore, <code>Image</code>s obtained from
 * an <code>ImageOp</code> <em>must not</em> be altered, as this might
 * interfere with image caching. If an <code>Image</code> obtained this way
 * needs to be modified, copy the <code>Image</code> first and alter the
 * copy.</p>
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
@SuppressFBWarnings(value = "NM_SAME_SIMPLE_NAME_AS_SUPERCLASS")
public abstract class AbstractOpImpl
  extends VASSAL.tools.opcache.AbstractOpImpl<BufferedImage>
  implements ImageOp {

  /** The cached size of this operation's resulting <code>Image</code>. */
  protected Dimension size;

  /** The cache which contains calculated <code>Image</code>s. */
  protected static final OpCache cache = new OpCache();

  public static void clearCache() {
    cache.clear();
  }

  public AbstractOpImpl() {
    super(cache);
  }

  /** {@inheritDoc} */
  @Override
  public abstract BufferedImage eval() throws Exception;

  /** {@inheritDoc} */
  @Override
  public BufferedImage getImage() {
    try {
      return getImage(null);
    }
    catch (CancellationException | InterruptedException e) {
      // FIXME: bug until we permit cancellation
      ErrorDialog.bug(e);
    }
    catch (ExecutionException e) {
      if (!Op.handleException(e)) ErrorDialog.bug(e);
    }

    return null;
  }

  /** {@inheritDoc} */
  @Override
  public BufferedImage getImage(ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException {

    return get(obs);
  }

  /** {@inheritDoc} */
  @Override
  public Future<BufferedImage> getFutureImage(ImageOpObserver obs)
                                                    throws ExecutionException {
    return getFuture(obs);
  }

  /**
   * A utility method for retrieving the size of the computed
   * <code>Image</code> from the cache if the <code>Image</code>
   * is cached.
   *
   * @return the size of the cached <code>Image</code>, or
   * <code>null</code> if the <code>Image</code> isn't cached
   */
  protected Dimension getSizeFromCache() {
    final BufferedImage im = cache.getIfDone(newKey());
    return im == null ? null : new Dimension(im.getWidth(), im.getHeight());
  }

  /**
   * Sets the <code>size</code> which is used by {@link #getSize},
   * {@link #getHeight}, and {@link #getWidth}.
   */
  protected abstract void fixSize();

  /** {@inheritDoc} */
  @Override
  public Dimension getSize() {
    if (size == null) fixSize();
    return new Dimension(size);
  }

  /** {@inheritDoc} */
  @Override
  public int getWidth() {
    if (size == null) fixSize();
    return size.width;
  }

  /** {@inheritDoc} */
  @Override
  public int getHeight() {
    if (size == null) fixSize();
    return size.height;
  }

  /** {@inheritDoc} */
  @Override
  public abstract Dimension getTileSize();

  /** {@inheritDoc} */
  @Override
  public abstract int getTileHeight();

  /** {@inheritDoc} */
  @Override
  public abstract int getTileWidth();

  /** {@inheritDoc} */
  @Override
  public abstract int getNumXTiles();

  /** {@inheritDoc} */
  @Override
  public abstract int getNumYTiles();

  /** {@inheritDoc} */
  @Override
  public BufferedImage getTile(Point p, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException {

    return getTile(p.x, p.y, obs);
  }

  /** {@inheritDoc} */
  @Override
  public abstract BufferedImage getTile(int tileX, int tileY,
                                        ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  /** {@inheritDoc} */
  @Override
  public Future<BufferedImage> getFutureTile(Point p, ImageOpObserver obs)
    throws ExecutionException {

    return getFutureTile(p.x, p.y, obs);
  }

  /** {@inheritDoc} */
  @Override
  public abstract Future<BufferedImage> getFutureTile(
    int tileX, int tileY, ImageOpObserver obs) throws ExecutionException;

  /** {@inheritDoc} */
  @Override
  public ImageOp getTileOp(Point p) {
    return getTileOp(p.x, p.y);
  }

  /** {@inheritDoc} */
  @Override
  public abstract ImageOp getTileOp(int tileX, int tileY);

  /** {@inheritDoc} */
  @Override
  public abstract Point[] getTileIndices(Rectangle rect);
}
