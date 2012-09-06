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
package VASSAL.tools.swing;

import java.awt.EventQueue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;

import VASSAL.tools.concurrent.RunnableFuture;

/**
 * An {@link ExecutorService} which submits to Event Dispatch Thread.
 *
 * @author Joel Uckelman
 * @since 3.2.0
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

  // FIXME: rename to newTaskFor(), mark as @Override in Java 1.6+
  /** {@inheritDoc} */
  protected <T> RunnableFuture<T> newTask(final Callable<T> cable) {
    return new EDTRunnableFuture<T>() {
      protected void runOnEDT() throws Exception {
        result = cable.call();
      }
    };
  }

  // FIXME: rename to newTaskFor(), mark as @Override in Java 1.6+
  /** {@inheritDoc} */
  protected <T> RunnableFuture<T> newTask(final Runnable rable, T result) {
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
    final RunnableFuture<Void> ftask = newTask(task, null);
    execute(ftask);
    return ftask;
  }

  // FIXME: remove for Java 1.6+
  /** {@inheritDoc} */
  @Override
  public <T> Future<T> submit(Runnable task, T result) {
    if (task == null) throw new NullPointerException();
    final RunnableFuture<T> ftask = newTask(task, result);
    execute(ftask);
    return ftask;
  }

  // FIXME: remove for Java 1.6+
  /** {@inheritDoc} */
  @Override
  public <T> Future<T> submit(Callable<T> task) {
    if (task == null) throw new NullPointerException();
    final RunnableFuture<T> ftask = newTask(task);
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

  // FIXME: remove for Java 1.6+
  /** {@inheritDoc} */
  public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks)
                                                  throws InterruptedException {
    if (tasks == null) throw new NullPointerException();

    final List<Future<T>> futures = new ArrayList<Future<T>>(tasks.size());
    boolean done = false;
    try {
      for (Callable<T> t : tasks) {
        final RunnableFuture<T> f = newTask(t);
        futures.add(f);
        execute(f);
      }

      for (Future<T> f : futures) {
        if (!f.isDone()) {
          try {
            f.get();
          }
          catch (CancellationException ignore) {
          }
          catch (ExecutionException ignore) {
          }
        }
      }

      done = true;
      return futures;
    }
    finally {
      if (!done) {
        for (Future<T> f : futures) f.cancel(true);
      }
    }
  }

  // FIXME: remove for Java 1.6+
  /** {@inheritDoc} */
  public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks,
                                       long timeout, TimeUnit unit)
                                                  throws InterruptedException {
    if (tasks == null) throw new NullPointerException();
    if (unit == null) throw new NullPointerException();

    long nanos = unit.toNanos(timeout);
    final List<Future<T>> futures = new ArrayList<Future<T>>(tasks.size());
    boolean done = false;
    try {
      for (Callable<T> t : tasks) futures.add(newTask(t));

      long lastTime = System.nanoTime();

      // Interleave time checks and calls to execute in case
      // executor doesn't have any/much parallelism.
      final Iterator<Future<T>> it = futures.iterator();
      while (it.hasNext()) {
        execute((Runnable)(it.next()));
        final long now = System.nanoTime();
        nanos -= now - lastTime;
        lastTime = now;
        if (nanos <= 0) return futures;
      }

      for (Future<T> f : futures) {
        if (!f.isDone()) {
          if (nanos <= 0) return futures;

          try {
            f.get(nanos, TimeUnit.NANOSECONDS);
          }
          catch (CancellationException ignore) {
          }
          catch (ExecutionException ignore) {
          }
          catch (TimeoutException toe) {
            return futures;
          }

          final long now = System.nanoTime();
          nanos -= now - lastTime;
          lastTime = now;
        }
      }

      done = true;
      return futures;
    }
    finally {
      if (!done) {
        for (Future<T> f : futures) f.cancel(true);
      }
    }
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
  protected final EDTRunnableFuture<Void> poison_pill =
                                                new EDTRunnableFuture<Void>() {
    protected void runOnEDT() {}
  };
}
