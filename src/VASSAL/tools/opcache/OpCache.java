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

package VASSAL.tools.opcache;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.jdesktop.swingworker.SwingWorker;

import VASSAL.tools.ErrorDialog;
import VASSAL.tools.concurrent.ConcurrentSoftHashMap;

/**
 * A memory-sensitive cache for {@link Op}s and their results.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class OpCache {

  /**
   * A cache key for <code>OpCache</code>.
   */
  public static class Key<V> {
    public final Op<V> op;
    public final int version;
    public final List<Key<?>> deps = new ArrayList<Key<?>>();

    private final int hash;

    /**
     * Creates a new key for the given <code>Op</code> and version.
     *
     * @param op the <code>Op</code> for which this is a key
     * @param version the current version of this key
     */
    public Key(Op<V> op, int version) {
      this.op = op;
      this.version = version;

      for (Op<?> dop : op.getSources()) deps.add(dop.newKey());

      hash = op.hashCode() ^ version ^ deps.hashCode();
    }

    /** {@inheritDoc} */
    @Override
    public boolean equals(Object o) {
      if (o == this) return true;
      if (o.getClass() != this.getClass()) return false;

      final Key<?> k = (Key<?>) o;
      return version == k.version &&
             op.equals(k.op) &&
             deps.equals(k.deps);
    }

    /** {@inheritDoc} */
    @Override
    public int hashCode() {
      return hash;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
      return this.getClass().getName() +
        "[op=" + op + ",version=" + version + "]";
    }
  }

  protected final ConcurrentMap<Key<?>,Future<?>> cache =
    new ConcurrentSoftHashMap<Key<?>,Future<?>>();

  /**
   * A request for execution of an {@link Op} which will be completed
   * synchronously and set manually.
   */
  private static final class Result<V> implements Future<V> {
    private static final long serialVersionUID = 1L;

    private V value = null;
    private boolean failed = false;
    private final CountDownLatch done = new CountDownLatch(1);

    public void set(V value) {
      this.value = value;
      done.countDown();
    }

    public void fail() {
      failed = true;
    }

    public boolean cancel(boolean mayInterruptIfRunning) {
      return false;
    }

    public boolean isCancelled() {
      return false;
    }

    public boolean isDone() {
      return done.getCount() == 0;
    }

    public V get() throws InterruptedException, ExecutionException {
      done.await();
      if (failed) throw new ExecutionException(new OpFailedException());
      return value;
    }

    public V get(long timeout, TimeUnit unit) throws InterruptedException,
                                                     ExecutionException,
                                                     TimeoutException
    {
      if (done.await(timeout, unit)) {
        if (failed) throw new ExecutionException(new OpFailedException());
        return value;
      }
      throw new TimeoutException();
    }
  }

  /**
   * The {@link Future} which is cached on failure of an {@link Op}.
   */
  private static final Future<Void> failure = new Future<Void>() {
    public boolean cancel(boolean mayInterruptIfRunning) {
      return false;
    }

    public Void get() throws ExecutionException {
      throw new ExecutionException(new OpFailedException());
    }

    public Void get(long timeout, TimeUnit unit) throws ExecutionException {
      throw new ExecutionException(new OpFailedException());
    }

    public boolean isCancelled() {
      return false;
    }

    public boolean isDone() {
      return true;
    }
  };

  /**
   * A request for execution of an {@link Op}, to be queued.
   */
  private class Request<V> extends SwingWorker<V,Void> {
    private final Key<V> key;
    private final OpObserver<V> obs;

    public Request(Key<V> key, OpObserver<V> obs) {
      if (key == null) throw new IllegalArgumentException();
      if (obs == null) throw new IllegalArgumentException();

      this.key = key;
      this.obs = obs;
    }

    @Override
    protected V doInBackground() throws Exception {
      return key.op.eval();
    }

    @Override
    protected void done() {
      try {
        final V val = get();
        if (obs != null) obs.succeeded(key.op, val);
      }
      catch (CancellationException e) {
        cache.remove(key, this);
        if (obs != null) obs.cancelled(key.op, e);
      }
      catch (InterruptedException e) {
        cache.remove(key, this);
        if (obs != null) obs.interrupted(key.op, e);
      }
      catch (ExecutionException e) {
        cache.replace(key, this, failure);
        if (obs != null) obs.failed(key.op, e);
      }
    }
  }

  /**
   * Gets a value from the cache.
   *
   * @param key the <code>Key</code> for which to retrieve a value
   * @return the value associated with <code>key</code>
   */
  public <V> V get(Key<V> key) {
    try {
      return get(key, null);
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
   * Gets a value from the cache, possibly asynchronously.
   *
   * @param key the <code>Key</code> for which to retrieve a value
   * @param obs the <code>OpObserver</code> to notify when the value is
   * available
   * @return the value associated with <code>key</code>
   *
   * @throws CancellationException if the request is cancelled
   * @throws InterruptedException if the request is interrupted
   * @throws ExecutionException if the request fails
   */
  public <V> V get(Key<V> key, OpObserver<V> obs) throws CancellationException,
                                                         InterruptedException,
                                                         ExecutionException
  {
    Future<V> fut = getFuture(key, obs);

    // We block on the op when there is no observer, and
    // return right away if the op is already done.
    if (obs == null || fut.isDone()) {
      try {
        return fut.get();
      }
      catch (CancellationException e) {
        cache.remove(key, fut);
        throw (CancellationException) new CancellationException().initCause(e);
      }
      catch (InterruptedException e) {
        cache.remove(key, fut);
        throw (InterruptedException) new InterruptedException().initCause(e);
      }
      catch (ExecutionException e) {
        cache.replace(key, fut, failure);
        throw new ExecutionException(e);
      }
    }

    // We have an observer and the op is still running.
    return null;
  }

  /**
   * Gets a {@link Future} from the cache. If <code>obs == null</code>, then
   * the {@link Op} associated with <code>key</code> will be executed
   * synchronously, and asynchronously otherwise.
   *
   * @param key the <code>Key</code> for which to retrieve a
   *    <code>Future</code>
   * @param obs the <code>OpObserver</code> to notify when the value is
   *    available
   * @return the <code>Future</code> associated with <code>key</code>
   *
   * @throws ExecutionException if the request is synchronous and fails
   */
  @SuppressWarnings("unchecked")
  public <V> Future<V> getFuture(Key<V> key, OpObserver<V> obs)
                                                    throws ExecutionException
  {
    // The code in this method was inspired by the article at
    // http://www.javaspecialists.eu/archive/Issue125.html.

    Future<V> fut = (Future<V>) cache.get(key);
    if (fut == null) {
      if (obs == null) {
        // check whether any other op has beat us into the cache
        final Result<V> res = new Result<V>();
        fut = (Future<V>) cache.putIfAbsent(key, res);

        // if not, then apply the op
        if (fut == null) {
          V val = null;
          try {
            val = key.op.eval();
          }
          catch (Throwable t) {
            res.fail();
            cache.put(key, failure);
            throw new ExecutionException(t);
          }
          finally {
            res.set(val);
          }

          fut = res;
        }
      }
      else {
        final Request<V> req = new Request<V>(key, obs);
        fut = (Future<V>) cache.putIfAbsent(key, req);
        if (fut == null) {
          threadPool.submit(req);
          fut = req;
        }
      }
    }
    else {
      // Are we a synchronous request in the queue being re-requested?
      if (obs == null && fut instanceof Runnable) {
        if (requestQueue.remove(fut)) {
          // Then run on this thread to prevent deadlock.
          ((Runnable) fut).run();
        }
      }
    }

    return fut;
  }

/////
// FIXME: finalize this...
  private final BlockingQueue<Runnable> requestQueue =
    new LinkedBlockingQueue<Runnable>();

  private static class Ex extends ThreadPoolExecutor {
    public Ex(int corePoolSize, int maximumPoolSize, long keepAliveTime,
              TimeUnit unit, BlockingQueue<Runnable> workQueue) {
      super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue);
    }

    public <V> Future<V> submit(SwingWorker<V,?> req) {
      execute(req);
      return req;
    }
  }

  private final Ex threadPool =
    new Ex(2, 2, 60, TimeUnit.SECONDS, requestQueue);
/////

  /**
   * Gets a value from the cache, if it is already calculated.
   *
   * @param key the <code>Key</code> for which to retrieve a value
   * @return the value associated with <code>key</code>, or <code>null</code>
   */
  @SuppressWarnings("unchecked")
  public <V> V getIfDone(Key<V> key) {
    final Future<V> fut = (Future<V>) cache.get(key);
    if (fut != null && fut.isDone()) {
      try {
        return fut.get();
      }
      catch (CancellationException e) {
      }
      catch (InterruptedException e) {
      }
      catch (ExecutionException e) {
      }
    }
    return null;
  }

  public void clear() {
// FIXME: should cancel all pending requests?
    cache.clear();
  }
}
