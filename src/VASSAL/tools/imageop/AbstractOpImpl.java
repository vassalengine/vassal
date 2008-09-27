/*
 * $Id$
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
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

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
public abstract class AbstractOpImpl
  extends VASSAL.tools.opcache.AbstractOpImpl<Image> implements ImageOp {

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

  /**
   * The image computation itself happens in this method.
   *
   * <p><b>Warning:</b> This method is not intended to be called from
   * anywhere except {@link #getImage}.</p>
   *
   * @throws Exception The operation represented by this <code>ImageOp</code>
   * could be anything, so any exception may be thrown.
   */
  public abstract Image apply() throws Exception;

  public Image eval() throws Exception {
    return apply();
  }

  /**
   * Calculates the <code>Image</code> produced by this operation. Calls
   * to this method are memoized to prevent redundant computations.
   *
   * <p><b>Warning:</b> <code>Image</code>s returned by this method
   * <em>must not</em> be modified.</p>
   *
   * @return the resulting <code>Image</code>
   */
  public Image getImage() {
    try {
      return getImage(null);
    }
    catch (CancellationException e) {
      // FIXME: bug until we permit cancellation 
      ErrorDialog.bug(e);
    }
    catch (InterruptedException e) {
      ErrorDialog.bug(e);
    }
    catch (ExecutionException e) {
      ErrorDialog.bug(e);
    }

    return null;
  }

  /**
   * Calculates the <code>Image</code> produced by this operation, and
   * reports completion or failure to the specified
   * <code>ImageOpObserver</code>. Calls to this method are memoized
   * to prevent redundant computations. If a non-<code>null</code> observer
   * is given, then the operation may be done asynchronously. If the
   * observer is <code>null</code>, then this method will block on
   * completion of the operation.
   * 
   * <p> When a non-blocking call is made (i.e., when
   * <code>obs != null</code>), the cache is checked and if the image is
   * found, it is returned immediately. If the image is already being
   * calculated, <code>obs</code> is notified when the pre-existing request
   * completes. Otherwise, a new request is queued and <code>obs</code> is
   * notified when that completes.</p>
   *
   * <p>When a blocking call is made (i.e., when <code>obs == null</code>),
   * the cache is checked and if the image is found, it is returned
   * immediately. If the image is already being calculated, this method
   * blocks on the completion of the existing calculation. Otherwise,
   * a new calculation is started and this method blocks on it. In
   * all cases, when a calculation is completed, the result is cached.</p>
   * 
   * <p><b>Warning:</b> <code>Image</code>s returned by this method
   * <em>must not</em> be modified.</p>
   *
   * @param obs the observer to be notified on completion
   * @return the resulting <code>Image</code>
   * @see #getTile
   * @see #getFutureTile
   * @see #getFutureImage
   */
  public Image getImage(ImageOpObserver obs) throws CancellationException,
                                                    InterruptedException,
                                                    ExecutionException
  {
    return get(obs);
  }

  /**
   * Submits a request for the <code>Image</code> produced by this operation,
   * and returns a reference to that request.
   * 
   * If a non-<code>null</code> observer is given, then the operation may
   * be done asynchronously. If the observer is <code>null</code>, then
   * this method will block on completion of the operation.
   *
   * <p>This implementaion uses a memory-sensitive cache to memoize
   * calls to <code>getFutureImage</code>. It returns a
   * {@code Future<Image>} so that the request may be cancelled if no
   * longer needed.</p>
   * 
   *  <p><code>Future</code>s are returned immediately, except in the
   * case where the is no observer and no pre-existing <code>Future</code>
   * for this <code>ImageOp</code>'s <code>Image</code>, in which case
   * this method blocks on completion of the computation.</p>
   *
   * <p><b>Warning:</b> <code>Image</code>s obtained from the
   * <code>Future</code>s returned by this method <em>must not</em> be
   * modified.</p>
   *
   * @param obs the observer to be notified on completion
   * @return a <code>Future</code> for the resulting <code>Image</code>
   * @see #getTile
   * @see #getFutureTile
   * @see #getImage
   */
  public Future<Image> getFutureImage(ImageOpObserver obs)
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
    final Image im = cache.getIfDone(newKey());
    return im == null ? null :
      new Dimension(im.getWidth(null), im.getHeight(null));
  }

  /**
   * Sets the <code>size</code> which is used by {@link getSize},
   * {@link getHeight}, and {@link getWidth}.
   */
  protected abstract void fixSize();

  /**
   * Returns the size of the <code>Image</code> which would be returned
   * by {@link #getImage}. The size is cached so that it need not be
   * recalculated on each call.
   *
   * @return the size of the resulting <code>Image</code> in pixels
   * @see #getHeight
   * @see #getWidth
   * iii
   */
  public Dimension getSize() {
    if (size == null) fixSize();
    return new Dimension(size);
  }

  /**
   * Returns the width of the <code>Image</code> which would be returned
   * by {@link #getImage}. The width is cached so that it need not be
   * recalculated on each call.
   *
   * @return the width of the resulting <code>Image</code> in pixels
   * @see #getHeight
   * @see #getSize
   */
  public int getWidth() {
    if (size == null) fixSize();
    return size.width;
  }

 /**
   * Returns the height of the <code>Image</code> which would be returned
   * by {@link #getImage}. The height is cached so that it need not be
   * recalculated on each call.
   *
   * @return the height of the resulting <code>Image</code> in pixels
   * @see #getWidth
   * @see #getSize
   */
  public int getHeight() {
    if (size == null) fixSize();
    return size.height;
  }

  /**
   * Returns the standard size of the <code>Image</code> tiles which are
   * returned by {@link #getTile}. Tiles which are in the extreme right
   * column will not have full width if the <code>Image</code> width
   * is not an integral multiple of the tile width. Similarly, tiles in
   * the bottom row will not have full height if the <code>Image</code>
   * height is not an integral multiple of the tile height.
   *
   * @return the size of <code>Image</code> tiles in pixels
   * @see #getTileHeight
   * @see #getTileWidth
   */
  public abstract Dimension getTileSize();

  /**
   * Returns the standard height of the <code>Image</code> tiles which are
   * returned by {@link #getTile}.
   *
   * @return the height of <code>Image</code> tiles in pixels
   * @see #getTileSize
   * @see #getTileWidth
   */
  public abstract int getTileHeight();

  /**
   * Returns the standard width of the <code>Image</code> tiles which are
   * returned by {@link #getTile}.
   *
   * @return the width of <code>Image</code> tiles in pixels
   * @see #getTileSize
   * @see #getTileHeight
   */
  public abstract int getTileWidth();

  /**
   * Returns the number of tiles along the x-axis. There will always be at
   * least one column of tiles. The number of columns <em>should</em>
   * equal <code>(int) Math.ceil((double) getWidth() / getTileWidth())</code>.
   *
   * @return the number of tiles along the x-axis
   */
  public abstract int getNumXTiles();

  /**
   * Returns the number of tiles along the y-axis. There will always be at
   * least one row of tiles. The number of rows <em>should</em> equal
   * <code>(int) Math.ceil((double) getHeight() / getTileHeight())</code>.
   *
   * @return the number of tiles along the y-axis
   */
  public abstract int getNumYTiles();

  /**
   * Calculates tile <code>(p.x,p.y)</code>, and reports
   * completion or failure to the specified <code>ImageOpObserver</code>.
   * If a non-<code>null</code> observer is given, then the operation may
   * be done asynchronously. If the observer is <code>null</code>, then
   * this method will block on completion of the operation. Tiles are
   * numbered from zero, so the tile in the upper-left corner of the main
   * <code>Image</code> is <code>(0,0)</code>. Note that <code>p.x</code>
   * and <code>p.y</code> are indices into the tile array, not pixel
   * locations.
   *
   * <p>This convenience method is equivalent to
   * <code>getTile(p.x, p.y, obs)</code>.</p>
   *
   * <p><b>Warning:</b> <code>Image</code>s returned by this method
   * <em>must not</em> be modified.</p>
   *
   * @param p the tile at position <code>(p.x,p.y)</code>
   * @param obs the observer
   * @return the resulting <code>Image</code>
   */
  public Image getTile(Point p, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException {
    return getTile(p.x, p.y, obs);
  }

  /**
   * Calculates tile <code>(tileX,tileY)</code>, and reports
   * completion or failure to the specified <code>ImageOpObserver</code>.
   * If a non-<code>null</code> observer is given, then the operation may
   * be done asynchronously. If the observer is <code>null</code>, then
   * this method will block on completion of the operation. Tiles are
   * numbered from zero, so the tile in the upper-left corner of the main
   * <code>Image</code> is <code>(0,0)</code>. Note that <code>tileX</code>
   * and <code>tileY</code> are indices into the tile array, not pixel
   * locations.
   *
   * <p><b>Warning:</b> <code>Image</code>s returned by this method
   * <em>must not</em> be modified.</p>
   *
   * @param tileX the x position of the requested tile
   * @param tileY the y position of the requested tile
   * @param obs the observer to be notified on completion
   * @return the resulting <code>Image</code>
   */
  public abstract Image getTile(int tileX, int tileY, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  /**
   * Submits a request for tile <code>(tileX,tileY)</code>, and returns a
   * reference to that request. If a non-<code>null</code> observer is given,
   * then the operation may be done asynchronously. If the observer is
   * <code>null</code>, then this method will block on completion of the
   * operation. Tiles are numbered from zero, so the tile in the upper-left
   * corner of the main <code>Image</code> is <code>(0,0)</code>. Note that
   * <code>tileX</code> and <code>tileY</code> are indices into the tile
   * array, not pixel locations.
   * 
   * <p>This convenience method is equivalent to
   * <code>getFutureTile(p.x, p.y, obs)</code>.</p>
   *
   * <p><b>Warning:</b> <code>Image</code>s obtained from the
   * <code>Future</code>s returned by this method <em>must not</em> be
   * modified.</p>
   *
   * @param tileX the x position of the requested tile
   * @param tileY the y position of the requested tile
   * @param obs the observer to be notified on completion
   * @return a <code>Future</code> for the resulting <code>Image</code>
   */
  public Future<Image> getFutureTile(Point p, ImageOpObserver obs)
    throws ExecutionException {
    return getFutureTile(p.x, p.y, obs);
  }

  /**
   * Submits a request for tile <code>(tileX,tileY)</code>, and returns a
   * reference to that request. If a non-<code>null</code> observer is given,
   * then the operation may be done asynchronously. If the observer is
   * <code>null</code>, then this method will block on completion of the
   * operation. Tiles are numbered from zero, so the tile in the upper-left
   * corner of the main <code>Image</code> is <code>(0,0)</code>. Note that
   * <code>tileX</code> and <code>tileY</code> are indices into the tile
   * array, not pixel locations.
   *
   * <p><b>Warning:</b> <code>Image</code>s obtained from the
   * <code>Future</code>s returned by this method <em>must not</em> be
   * modified.</p>
   *
   * @param tileX the x position of the requested tile
   * @param tileY the y position of the requested tile
   * @param obs the observer to be notified on completion
   * @return a <code>Future</code> for the resulting <code>Image</code>
   */
  public abstract Future<Image> getFutureTile(int tileX, int tileY,
                                              ImageOpObserver obs)
    throws ExecutionException;

  public ImageOp getTileOp(Point p) {
    return getTileOp(p.x, p.y);
  }

  public abstract ImageOp getTileOp(int tileX, int tileY);

  /**
   * Returns an array of <code>Point</code>s representing the tiles
   * intersecting the given <code>Rectangle</code>. 
   * 
   * @param rect the rectangle
   * @return the positions of the tiles hit by the rectangle
   * @throws IllegalArgumentException if <code>rect == null</code>.
   */
  public abstract Point[] getTileIndices(Rectangle rect);
}
