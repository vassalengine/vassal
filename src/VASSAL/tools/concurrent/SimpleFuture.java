/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman
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
import java.util.concurrent.Future;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.TimeUnit;

/**
 * An uninterruptable {@link Future}.
 *
 * @param <V> the result type returned by the {@link #get} method
 * @author Joel Uckelman
 * @since 3.1.11
 */
public class SimpleFuture<V> implements Future<V> {
  protected V value = null;
  private Throwable exception = null;
  private volatile boolean cancelled = false;

  private final CountDownLatch done = new CountDownLatch(1); 

  /** 
   * {@inheritDoc}
   * 
   * <p>This implementation is uninteruptable, so ignores the parameter.</p>
   */
  public boolean cancel(boolean mayInterruptIfRunning) {
    if (!isDone()) cancelled = true; 
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

  /**
   * Sets the exception to be wrapped in an {@link ExecutionException} and
   * thrown by {@link #get}.
   *
   * <b>May be called only from the thread executing the computation.</b>
   *
   * @param t the exception
   */
  public void setException(Throwable t) {
    exception = t;
    done.countDown();
  }

  /**
   * Sets the result to be returned by {@link #get}.
   *
   * <b>May be called only from the thread executing the computation.</b>
   *
   * @param v the value
   */
  public void set(V v) {
    value = v;
    done.countDown();
  }
 
  /** {@inheritDoc} */
  public V get() throws CancellationException,
                        ExecutionException,
                        InterruptedException {
    done.await();
    if (exception != null) throw new ExecutionException(exception);
    if (cancelled) throw new CancellationException();
    return value;
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
    return value;
  }
}
