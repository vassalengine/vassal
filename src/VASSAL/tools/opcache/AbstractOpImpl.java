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

import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 * An abstract representation of an operation. <code>AbstractOpImpl</code>
 * is the base class for all such operations. The results of all operations
 * are memoized (using a memory-sensitive cache), so retrieving results is
 * both fast and memory-efficient.
 *
 * <p><b>Warning:</b> For efficiency reasons, the methods {@link #get()} and
 * {@link #get(OpObserver)} do <em>not</em> return defensively, nor do the
 * {@code Future}s returned by {@link #getFuture(OpObserver)}. That is, the
 * object returned is possibly the one retained internally by the
 * <code>AbstractOpImpl</code>. Therefore, objects obtained from an
 * <code>AbstractOpImpl</code> <em>must not</em> be altered, as this might
 * interfere with caching. If an object obtained this way needs to be
 * modified, copy the object first and alter the copy.</p>
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public abstract class AbstractOpImpl<V> implements Op<V> {
  /** The cache which contains calculated results. */
  protected final OpCache cache;

  /**
   * @param cache the cache for storing our result
   */
  public AbstractOpImpl(OpCache cache) {
    this.cache = cache;
  }

  /** {@inheritDoc} */
  public abstract List<Op<?>> getSources();

  /** {@inheritDoc} */
  public abstract V eval() throws Exception;

  /** {@inheritDoc} */
  public V get() {
    return cache.get(newKey());
  }

  /**
   * {@inheritDoc}
   *
   * @throws CancellationException if the operation was cancelled
   * @throws InterruptedException if the operation was interrupted
   * @throws ExecutionException if the operation failed
   */
  public V get(OpObserver<V> obs) throws CancellationException,
                                         InterruptedException,
                                         ExecutionException {
    return cache.get(newKey(), obs);
  }

  /**
   * {@inheritDoc}
   *
   * @throws CancellationException if the operation was cancelled
   * @throws InterruptedException if the operation was interrupted
   * @throws ExecutionException if the operation failed
   */
  public Future<V> getFuture(OpObserver<V> obs) throws ExecutionException {
    return cache.getFuture(newKey(), obs);
  }

  private static final ConcurrentMap<Op<?>,OpCache.Key<?>> kcache =
    new ConcurrentHashMap<Op<?>,OpCache.Key<?>>();

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public OpCache.Key<V> newKey() {
    OpCache.Key<V> key = (OpCache.Key<V>) kcache.get(this);
    if (key == null) {
      final OpCache.Key<V> nkey = new OpCache.Key<V>(this, 0);
      key = (OpCache.Key<V>) kcache.putIfAbsent(this, nkey);
      if (key == null) key = nkey;
    }

    return key;
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public void update() {
    final OpCache.Key<V> key = (OpCache.Key<V>) kcache.get(this);
    if (key != null) {
      final OpCache.Key<V> nkey = new OpCache.Key<V>(this, key.version+1);
      kcache.replace(this, key, nkey);
    }
  }
}
