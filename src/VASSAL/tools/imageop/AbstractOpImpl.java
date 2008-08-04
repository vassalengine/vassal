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
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.locks.AbstractQueuedSynchronizer;

import org.jdesktop.swingworker.SwingWorker;

import VASSAL.tools.ConcurrentSoftHashMap;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThreadManager;

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
public abstract class AbstractOpImpl implements ImageOp {
  /** The cached size of this operation's resulting <code>Image</code>. */
  protected Dimension size;

  /** The cache which contains calculated <code>Image</code>s. */
  protected static final ConcurrentMap<ImageOp,Future<Image>> cache =
    new ConcurrentSoftHashMap<ImageOp,Future<Image>>();

// NOTE: How to handle cancellation? In order to tell whether a Future
// can be cancelled, we need to know how many times it's been retrieved
// from the cache and handed out. We can do that by keeping count of
// how many times that happens. Everytime cancel() is called, we decrement
// the count. Once the count goes to 0, we can really cancel the request.
// Whenever we cancel a request, we should reduce the count for any
// requests it depends on, as well. (How to find those?)

  private static final Future<Image> failure = new Future<Image>() {
    public boolean cancel(boolean mayInterruptIfRunning) {
      return false;
    }

    public Image get() {
      return null;
    }

    public Image get(long timeout, TimeUnit unit) {
      return null;
    }

    public boolean isCancelled() {
      return false;
    }

    public boolean isDone() {
      return true;
    }
  };

  private static final class Waiter implements Future<Image> {
    private Image im = null;
    private boolean done = false;  
 
    private static final long serialVersionUID = 1L;
 
    private final Sync sync =  new Sync();
  
    public Waiter() {
      sync.acquire(1);
    }
  
    public void set(Image im) {
      this.im = im;
      this.done = true;
      sync.release(1);
    }
  
  // FIXME: properly implement cancel, isCancelled, isDone, timeout get 
    public boolean cancel(boolean mayInterruptIfRunning) {
      return false;
    }
  
    public boolean isCancelled() {
      return false;
    }
  
    public boolean isDone() {
      return done;
    }
  
    public Image get() throws InterruptedException, ExecutionException {
      sync.acquireShared(0);
      try {
        return im;
      }
      finally {
        sync.releaseShared(0);
      }
    }
  
  // FIXME: is this correct? 
    public Image get(long timeout, TimeUnit unit)
      throws InterruptedException, ExecutionException, TimeoutException {
      if (sync.tryAcquireSharedNanos(0, unit.toNanos(timeout))) {
        try {
          return im;
        }
        finally {
          sync.releaseShared(0);
        }
      }
      throw new TimeoutException();
    }
   
    private static class Sync extends AbstractQueuedSynchronizer {
      private static final long serialVersionUID = 1L;

      protected boolean tryAcquire(int acquires) {
        if (compareAndSetState(0,1)) {
// FIXME: reinstate this once we move to 1.6+.
//          setExclusiveOwnerThread(Thread.currentThread());
          return true;
        }
        return false;
      }
  
      protected boolean tryRelease(int releases) {
        if (getState() == 0) throw new IllegalMonitorStateException();
// FIXME: reinstate this once we move to 1.6+.
//        setExclusiveOwnerThread(null);
        setState(0);
        return true;
      }
  
      protected int tryAcquireShared(int acquires) {
        return getState() == 0 ? 1 : -1;
      }
  
      protected boolean tryReleaseShared(int releases) {
        return getState() == 0;
      }
    }
  }

  private static final class Request extends SwingWorker<Image,Void> {
    private final ImageOp op;
    private final ImageOpObserver obs;
    
    public Request(ImageOp op, ImageOpObserver obs) {
      if (op == null) throw new IllegalArgumentException();
      if (obs == null) throw new IllegalArgumentException();

      this.op = op;
      this.obs = obs;
    }

    @Override
    protected Image doInBackground() throws Exception {
      return op.apply();
    }

    @Override
    protected void done() {
      // We catch get() exceptions here to handle the case where this
      // Request was returned by getFutureImage() or getFutureTile().

      try {
        get();
        obs.imageOpChange(op, true);
      }
      catch (CancellationException e) {
        // FIXME: should not call bug once we implement cancellation
        ErrorDialog.bug(e);
        cache.remove(op, this);
        obs.imageOpChange(op, false);
      }
      catch (InterruptedException e) {
        ErrorDialog.bug(e);
        cache.remove(op, this);
        obs.imageOpChange(op, false);
      }
      catch (ExecutionException e) {
        OpErrorDialog.error(e, op);
        cache.replace(op, this, failure);
        obs.imageOpChange(op, false);
      }
      // System.out.println("finishing, async: " + op);
    }
  }

  public static void clearCache() {
    cache.clear();  
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
  public Image getImage(ImageOpObserver obs)
    throws CancellationException, InterruptedException, ExecutionException {
    // The code in this method was inspired by the article at
    // http://www.javaspecialists.eu/archive/Issue125.html.

    Future<Image> fim = cache.get(this);

    if (obs == null) {  // no observer, block on the op
      // System.out.println("start, sync: " + this);
      if (fim == null) {
        // check whether any other op has beat us into the cache
        final Waiter of = new Waiter();
        fim = cache.putIfAbsent(this, of);

        // if not, then apply the op
        if (fim == null) {
          Image im = null;
          try {
            im = apply();
          }
          catch (Exception e) {
            // System.out.println("throwing, sync: " + this);
            cache.put(this, failure);
            throw new ExecutionException(e);
          }
          finally {
            of.set(im); 
          }

          // System.out.println("finishing, sync: " + this);
          return im;
/*
          catch (Exception e) {
System.out.println("throwing, sync: " + this);
            ErrorLog.log(e);
//            cache.remove(this, of);
            cache.put(this, failure);
            ErrorDialog.raiseErrorDialog(e, e.getMessage());
          }    

// NOTE: if we make getImage throw, we need to have a finally block here
// which includes:
//         cache.put(this,failure);
//         of.set(im);

          of.set(im);
          // System.out.println("finishing, sync: " + this);
          return im;
*/
        }
      }

      // the image already has a future, so block on it
      try {
        return fim.get();
        /*
          System.out.println("blocking: " + this);
          final Image im = fim.get();
          System.out.println("finishing, sync: " + this);
          return im;
        */
      }
      catch (CancellationException e) {
        cache.remove(this, fim);
        throw e;
      }
      catch (InterruptedException e) {
        cache.remove(this, fim);
        throw e;
      }
      catch (ExecutionException e) {
        cache.replace(this, fim, failure);
        throw e;
      }

//      return null;
/*
      catch (CancellationException e) {
System.out.println("throwing: " + this);
        cache.remove(this, fim);
        ErrorLog.log(e);
      }
      catch (InterruptedException e) {
System.out.println("throwing: " + this);
        cache.remove(this, fim);
        ErrorLog.log(e);
      }
      catch (ExecutionException e) {
System.out.println("throwing: " + this);
//        cache.remove(this, fim);
        ErrorLog.log(e);
      }
// NOTE: if we make getImage throw, we need to separate out the exceptions
// in which fim should be removed, and rethrow those.

      return null;
*/
    }
    else {  // we have an observer, don't block on the op
      // System.out.println("start, async: " + this);
      if (fim == null) {
        final Request req = new Request(this, obs);
        fim = cache.putIfAbsent(this, req);

        if (fim == null) {
          // System.out.println("submitting: " + this);
          ThreadManager.submit(req);
          return null;
// FIXME: what happens if req throws an exception internally?
        }
      }
        
      // check if the future is now
      if (fim.isDone()) {
        try {
          return fim.get();
        }
        catch (CancellationException e) {
          cache.remove(this, fim);
          throw e;
        }
        catch (InterruptedException e) {
          cache.remove(this, fim);
          throw e;
        }
        catch (ExecutionException e) {
          cache.replace(this, fim, failure);
          throw e;
        }

/*
        catch (CancellationException e) {
System.out.println("throwing: " + this);
          cache.remove(this, fim);
          ErrorLog.log(e);
        }
        catch (InterruptedException e) {
System.out.println("throwing: " + this);
          cache.remove(this, fim);
          ErrorLog.log(e);
        }
        catch (ExecutionException e) {
System.out.println("throwing: " + this);
//          cache.remove(this, fim);
          cache.replace(this, fim, failure);
          ErrorLog.log(e);
        }
*/
      }
// NOTE: if we make getImage throw, we need to separate out the exceptions
// in which fim should be removed from the replace case, and rethrow all
// of these.

      return null;
    }
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

    Future<Image> fim = cache.get(this);

    if (obs == null) {  // no observer
      // System.out.println("start, sync: " + this);
      if (fim == null) {
        // check whether any other op has beat us into the cache
        final Waiter of = new Waiter();
        fim = cache.putIfAbsent(this, of);

        // if not, then apply the op
        if (fim == null) {
          Image im = null;
          try {
            im = apply();
          }
          catch (Exception e) {
            cache.put(this, failure);
            throw new ExecutionException(e);
          }
          finally {
            of.set(im); 
          }

          fim = of;
/*
          catch (Exception e) {
System.out.println("throwing, sync: " + this);
            ErrorLog.log(e);
            cache.put(this, failure);
          }

          of.set(im);
          // System.out.println("finishing, sync: " + this);
          fim = of;
*/
        }
      }
    }
    else {  // we have an observer
      // System.out.println("start, async: " + this);
      if (fim == null) {
        final Request req = new Request(this, obs);
        fim = cache.putIfAbsent(this, req);

        if (fim == null) {
          // System.out.println("submitting: " + this);
          fim = ThreadManager.submit(req);
        }
      }
    }

    return fim;       
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
    final Future<Image> fim = cache.get(this);
    if (fim != null && fim.isDone()) {
      try {
        final Image im = fim.get();
        return im == null ? new Dimension() :
          new Dimension(im.getWidth(null), im.getHeight(null));
      }
      catch (CancellationException e) {
        // FIXME: a bug until we permit cancellation
        ErrorDialog.bug(e);
      }
      catch (InterruptedException e) {
        ErrorDialog.bug(e);
      }
      catch (ExecutionException e) {
        // ignore this, as this means that the size isn't cached
      }
    }
    return null;    
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
