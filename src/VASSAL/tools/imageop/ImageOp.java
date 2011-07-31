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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 * An abstract representation of an operation which may be applied to an
 * {@link Image}. <code>ImageOp</code> is the interface for all such
 * operations. The results of all operations are memoized (using a
 * memory-sensitive cache), so retrieving results is both fast and
 * memory-efficient.
 *
 * <p><b>Warning:</b> For efficiency reasons, no images retrieved from
 * an <code>ImageOp</code> are returned defensively. That is, the
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
public interface ImageOp extends VASSAL.tools.opcache.Op<BufferedImage> {

  /**
   * The image computation itself happens in this method.
   *
   * <p><b>Warning:</b> This method is not intended to be called from
   * anywhere except {@link #getImage}.</p>
   *
   * @throws Exception The operation represented by this <code>ImageOp</code>
   * could be anything, so any exception may be thrown.
   */
  public BufferedImage eval() throws Exception;

  /**
   * Calculates the <code>BufferedImage</code> produced by this operation.
   * Calls to this method are memoized to prevent redundant computations.
   *
   * <p><b>Warning:</b> <code>BufferedImage</code>s returned by this method
   * <em>must not</em> be modified.</p>
   *
   * @return the resulting <code>BufferedImage</code>
   */
  public BufferedImage getImage();

  /**
   * Calculates the <code>BufferedImage</code> produced by this operation, and
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
   * <p><b>Warning:</b> <code>BufferedImage</code>s returned by this method
   * <em>must not</em> be modified.</p>
   *
   * @param obs the observer to be notified on completion
   * @return the resulting <code>BufferedImage</code>
   * @throws CancellationException if the operation was cancelled
   * @throws InterruptedException if the operation was interrupted
   * @throws ExecutionException if the operation failed
   * @see #getTile
   * @see #getFutureTile
   * @see #getFutureImage
   */
  public BufferedImage getImage(ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  /**
   * Submits a request for the <code>BufferedImage</code> produced by this
   * operation, and returns a reference to that request.
   *
   * If a non-<code>null</code> observer is given, then the operation may
   * be done asynchronously. If the observer is <code>null</code>, then
   * this method will block on completion of the operation.
   *
   * <p>This implementaion uses a memory-sensitive cache to memoize
   * calls to <code>getFutureImage</code>. It returns a
   * {@code Future<BufferedImage>} so that the request may be cancelled if no
   * longer needed.</p>
   *
   *  <p><code>Future</code>s are returned immediately, except in the
   * case where the is no observer and no pre-existing <code>Future</code>
   * for this <code>ImageOp</code>'s <code>BufferedImage</code>, in which
   * case this method blocks on completion of the computation.</p>
   *
   * <p><b>Warning:</b> <code>BufferedImage</code>s obtained from the
   * <code>Future</code>s returned by this method <em>must not</em> be
   * modified.</p>
   *
   * @param obs the observer to be notified on completion
   * @return a <code>Future</code> for the resulting <code>BufferedImage</code>
   * @throws ExecutionException if the operation failed
   * @see #getTile
   * @see #getFutureTile
   * @see #getImage
   */
  public Future<BufferedImage> getFutureImage(ImageOpObserver obs)
    throws ExecutionException;

  /**
   * Returns the size of the <code>BufferedImage</code> which would be returned
   * by {@link #getImage}. The size is cached so that it need not be
   * recalculated on each call.
   *
   * @return the size of the resulting <code>BufferedImage</code> in pixels
   * @see #getHeight
   * @see #getWidth
   */
  public Dimension getSize();

  /**
   * Returns the width of the <code>BufferedImage</code> which would be
   * returned by {@link #getImage}. The width is cached so that it need not
   * be recalculated on each call.
   *
   * @return the width of the resulting <code>BufferedImage</code> in pixels
   * @see #getHeight
   * @see #getSize
   */
  public int getWidth();

  /**
   * Returns the height of the <code>BufferedImage</code> which would be
   * returned by {@link #getImage}. The height is cached so that it need
   * not be recalculated on each call.
   *
   * @return the height of the resulting <code>BufferedImage</code> in pixels
   * @see #getWidth
   * @see #getSize
   */
  public int getHeight();

  /**
   * Returns the standard size of the <code>BufferedImage</code> tiles
   * which are returned by {@link #getTile}. Tiles which are in the extreme
   * right column will not have full width if the <code>BufferedImage</code>
   * width is not an integral multiple of the tile width. Similarly, tiles in
   * the bottom row will not have full height if the <code>BufferedImage</code>
   * height is not an integral multiple of the tile height.
   *
   * @return the size of <code>BufferedImage</code> tiles in pixels
   * @see #getTileHeight
   * @see #getTileWidth
   */
  public Dimension getTileSize();

  /**
   * Returns the standard height of the <code>BufferedImage</code> tiles
   * which are returned by {@link #getTile}.
   *
   * @return the height of <code>BufferedImage</code> tiles in pixels
   * @see #getTileSize
   * @see #getTileWidth
   */
  public int getTileHeight();

  /**
   * Returns the standard width of the <code>BufferedImage</code> tiles which
   * are returned by {@link #getTile}.
   *
   * @return the width of <code>BufferedImage</code> tiles in pixels
   * @see #getTileSize
   * @see #getTileHeight
   */
  public int getTileWidth();

  /**
   * Returns the number of tiles along the x-axis. There will always be at
   * least one column of tiles. The number of columns <em>should</em>
   * equal <code>(int) Math.ceil((double) getWidth() / getTileWidth())</code>.
   *
   * @return the number of tiles along the x-axis
   */
  public int getNumXTiles();

  /**
   * Returns the number of tiles along the y-axis. There will always be at
   * least one row of tiles. The number of rows <em>should</em> equal
   * <code>(int) Math.ceil((double) getHeight() / getTileHeight())</code>.
   *
   * @return the number of tiles along the y-axis
   */
  public int getNumYTiles();

  /**
   * Calculates tile <code>(p.x,p.y)</code>, and reports
   * completion or failure to the specified <code>ImageOpObserver</code>.
   * If a non-<code>null</code> observer is given, then the operation may
   * be done asynchronously. If the observer is <code>null</code>, then
   * this method will block on completion of the operation. Tiles are
   * numbered from zero, so the tile in the upper-left corner of the main
   * <code>BufferedImage</code> is <code>(0,0)</code>. Note that
   * <code>p.x</code> and <code>p.y</code> are indices into the tile array,
   * not pixel locations.
   *
   * <p>This convenience method is equivalent to
   * <code>getTile(p.x, p.y, obs)</code>.</p>
   *
   * <p><b>Warning:</b> <code>BufferedImage</code>s returned by this method
   * <em>must not</em> be modified.</p>
   *
   * @param p the position of the requested tile
   * @param obs the observer
   * @return the resulting <code>BufferedImage</code>
   * @throws CancellationException if the operation was cancelled
   * @throws InterruptedException if the operation was interrupted
   * @throws ExecutionException if the operation failed
   */
  public BufferedImage getTile(Point p, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  /**
   * Calculates tile <code>(tileX,tileY)</code>, and reports
   * completion or failure to the specified <code>ImageOpObserver</code>.
   * If a non-<code>null</code> observer is given, then the operation may
   * be done asynchronously. If the observer is <code>null</code>, then
   * this method will block on completion of the operation. Tiles are
   * numbered from zero, so the tile in the upper-left corner of the main
   * <code>BufferedImage</code> is <code>(0,0)</code>. Note that
   * <code>tileX</code> and <code>tileY</code> are indices into the tile
   * array, not pixel locations.
   *
   * <p><b>Warning:</b> <code>BufferedImage</code>s returned by this method
   * <em>must not</em> be modified.</p>
   *
   * @param tileX the x position of the requested tile
   * @param tileY the y position of the requested tile
   * @param obs the observer to be notified on completion
   * @return the resulting <code>BufferedImage</code>
   * @throws CancellationException if the operation was cancelled
   * @throws InterruptedException if the operation was interrupted
   * @throws ExecutionException if the operation failed
   */
  public BufferedImage getTile(int tileX, int tileY, ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException;

  /**
   * Submits a request for tile <code>(tileX,tileY)</code>, and returns a
   * reference to that request. If a non-<code>null</code> observer is given,
   * then the operation may be done asynchronously. If the observer is
   * <code>null</code>, then this method will block on completion of the
   * operation. Tiles are numbered from zero, so the tile in the upper-left
   * corner of the main <code>BufferedImage</code> is <code>(0,0)</code>.
   * Note that <code>tileX</code> and <code>tileY</code> are indices into the
   * tile array, not pixel locations.
   *
   * <p>This convenience method is equivalent to
   * <code>getFutureTile(p.x, p.y, obs)</code>.</p>
   *
   * <p><b>Warning:</b> <code>BufferedImage</code>s obtained from the
   * <code>Future</code>s returned by this method <em>must not</em> be
   * modified.</p>
   *
   * @param p the position of the requested tile
   * @param obs the observer to be notified on completion
   * @return a <code>Future</code> for the resulting <code>BufferedImage</code>
   * @throws ExecutionException if the operation failed
   */
  public Future<BufferedImage> getFutureTile(Point p, ImageOpObserver obs)
    throws ExecutionException;

  /**
   * Submits a request for tile <code>(tileX,tileY)</code>, and returns a
   * reference to that request. If a non-<code>null</code> observer is given,
   * then the operation may be done asynchronously. If the observer is
   * <code>null</code>, then this method will block on completion of the
   * operation. Tiles are numbered from zero, so the tile in the upper-left
   * corner of the main <code>BufferedImage</code> is <code>(0,0)</code>.
   * Note that <code>tileX</code> and <code>tileY</code> are indices into the
   * tile array, not pixel locations.
   *
   * <p><b>Warning:</b> <code>BufferedImage</code>s obtained from the
   * <code>Future</code>s returned by this method <em>must not</em> be
   * modified.</p>
   *
   * @param tileX the x position of the requested tile
   * @param tileY the y position of the requested tile
   * @param obs the observer to be notified on completion
   * @return a <code>Future</code> for the resulting <code>BufferedImage</code>
   * @throws ExecutionException if the operation failed
   */
  public Future<BufferedImage> getFutureTile(
    int tileX, int tileY, ImageOpObserver obs) throws ExecutionException;

  /**
   * Returns an <code>ImageOp</code> which can produce the requested tile.
   *
   * <p>This convenience method is equivalent to
   * <code>getTileOp(p.x, p.y)</code>.</p>
   *
   * @param p the position of the requested tile
   * @return the <code>ImageOp</code> which produces the requested tile
   */
  public ImageOp getTileOp(Point p);

  /**
   * Returns an <code>ImageOp</code> which can produce the requested tile.
   *
   * @param tileX the x position of the requested tile
   * @param tileY the y position of the requested tile
   * @return the <code>ImageOp</code> which produces the requested tile
   */
  public ImageOp getTileOp(int tileX, int tileY);

  /**
   * Returns an array of <code>Point</code>s representing the tiles
   * intersecting the given <code>Rectangle</code>.
   *
   * @param rect the rectangle
   * @return the positions of the tiles hit by the rectangle
   * @throws IllegalArgumentException if <code>rect == null</code>.
   */
  public Point[] getTileIndices(Rectangle rect);
}
