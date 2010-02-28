/*
 * $Id: GameModule.java 5488 2009-04-11 14:01:20Z uckelman $
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

import java.awt.EventQueue;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;

/**
 * An {@link ExecutorService} which submits to Event Dispatch Thread.
 *
 * @author Joel Uckelman
 * @since 3.1.11
 * @see EDTRunnableFuture
 */
public class EDTExecutorService extends AbstractExecutorService {
  private final AtomicBoolean shutdown = new AtomicBoolean(false);
  private final ReentrantLock lock = new ReentrantLock();

  /** {@inheritDoc} */
  public boolean awaitTermination(long timeout, TimeUnit unit)
                                                  throws InterruptedException {
    // Wait for the poison pill to finish.
    lock.lock();
    try {
      if (isTerminated()) return true;
      poison_pill.get(timeout, unit);
      return true;
    }
    catch (TimeoutException e) {
      return false;
    }
    catch (CancellationException e) {
      // Should not happen, the poision pill is never cancelled.
      throw new IllegalStateException(e);
    }
    catch (ExecutionException e) {
      // Should not happen, the poision pill runs no code.
      throw new IllegalStateException(e);
    }
    finally {
      lock.unlock();
    }
  }

  /** {@inheritDoc} */
  public boolean isShutdown() {
    return shutdown.get();
  }

  /** {@inheritDoc} */
  public boolean isTerminated() {
    return shutdown.get() && poison_pill.isDone();
  }

  /** {@inheritDoc} */
  public void shutdown() {
    lock.lock();
    try {
      if (shutdown.get()) return;

      submit(poison_pill);
      shutdown.set(true);
    }
    finally {
      lock.unlock();
    }
  }
  
  /** {@inheritDoc} */
  public List<Runnable> shutdownNow() {
    shutdown();
    return Collections.<Runnable>emptyList();
  }

  // FIXME: mark as @Override in Java 1.6+
  /** {@inheritDoc} */
  protected <T> RunnableFuture<T> newTaskFor(final Callable<T> cable) {
    return new EDTRunnableFuture<T>() {
      protected void runOnEDT() throws Exception {
        value = cable.call();
      }
    };
  }

  // FIXME: mark as @Override in Java 1.6+
  /** {@inheritDoc} */
  protected <T> RunnableFuture<T> newTaskFor(final Runnable rable, T result) {
    return new EDTRunnableFuture<T>(result) {
      protected void runOnEDT() {
        rable.run();
      }
    };
  }

  // FIXME: remove for Java 1.6+
  /** {@inheritDoc} */
  @Override
  public Future<?> submit(Runnable task) {
    if (task == null) throw new NullPointerException();
    final RunnableFuture<Void> ftask = newTaskFor(task, null);
    execute(ftask);
    return ftask;
  }

  // FIXME: remove for Java 1.6+
  /** {@inheritDoc} */
  @Override
  public <T> Future<T> submit(Runnable task, T result) {
    if (task == null) throw new NullPointerException();
    final RunnableFuture<T> ftask = newTaskFor(task, result);
    execute(ftask);
    return ftask;
  }

  // FIXME: remove for Java 1.6+
  /** {@inheritDoc} */
  @Override
  public <T> Future<T> submit(Callable<T> task) {
    if (task == null) throw new NullPointerException();
    final RunnableFuture<T> ftask = newTaskFor(task);
    execute(ftask);
    return ftask;
  }

  /**
   * Submits a {@code EDTRunnableFuture} task for execution and returns it.
   * 
   * @param task the task to submit
   * @return the task which was submitted
   * @throws RejectedExecutionException if the task cannot be scheduled for
   *   execution
   * @throws NullPointerException if the task is {@code null}
   */
  public <T> EDTRunnableFuture<T> submit(EDTRunnableFuture<T> task) {
    execute(task);
    return task;
  }

  /** {@inheritDoc} */
  public void execute(Runnable r) {
    if (r == null) throw new NullPointerException();

    lock.lock();
    try {
      if (shutdown.get()) throw new RejectedExecutionException();

      EventQueue.invokeLater(r);
    }
    finally {
      lock.unlock();
    }
  }

  // The poision pill task used for shutting down the ExecutorService
  private static final EDTRunnableFuture<Void> poison_pill =
                                                new EDTRunnableFuture<Void>() {
    protected void runOnEDT() {}
  };
}
