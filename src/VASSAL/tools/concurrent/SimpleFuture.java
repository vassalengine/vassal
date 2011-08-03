/*
 * $Id$
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
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

package VASSAL.tools.concurrent;

import java.util.concurrent.CancellationException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * An uninterruptable {@link SettableFuture}.
 *
 * @param <V> the result type returned by the {@link #get} method
 * @author Joel Uckelman
 * @since 3.1.11
 */
public class SimpleFuture<V> implements SettableFuture<V> {
  protected V result = null;
  protected Throwable exception = null;
  protected volatile boolean cancelled = false;

  protected final CountDownLatch done = new CountDownLatch(1);

  /**
   * {@inheritDoc}
   *
   * <p>This implementation is uninteruptable, so ignores the parameter.</p>
   */
  public boolean cancel(boolean mayInterruptIfRunning) {
    // fail if already cancelled
    if (cancelled) return false;

    // cancel if running
    if (done.getCount() != 0) {
      cancelled = true;
      done.countDown();
    }

    return cancelled;
  }

  /** {@inheritDoc} */
  public boolean isCancelled() {
    return cancelled;
  }

  /** {@inheritDoc} */
  public boolean isDone() {
    return cancelled || done.getCount() == 0;
  }

  /** {@inheritDoc} */
  public void setException(Throwable t) {
    exception = t;
    done.countDown();
  }

  /** {@inheritDoc} */
  public void set(V r) {
    result = r;
    done.countDown();
  }

  /** {@inheritDoc} */
  public V get() throws CancellationException,
                        ExecutionException,
                        InterruptedException {
    done.await();
    if (exception != null) throw new ExecutionException(exception);
    if (cancelled) throw new CancellationException();
    return result;
  }

  /** {@inheritDoc} */
  public V get(long timeout, TimeUnit unit) throws CancellationException,
                                                   ExecutionException,
                                                   InterruptedException,
                                                   TimeoutException
  {
    if (!done.await(timeout, unit)) throw new TimeoutException();
    if (exception != null) throw new ExecutionException(exception);
    if (cancelled) throw new CancellationException();
    return result;
  }
}
