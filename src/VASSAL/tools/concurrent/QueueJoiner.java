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

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

/**
 * A {@link Callable} which calls a {@code Callable} retrieved from a queue.
 *
 * {@code QueueJoiner} permits one queue of {@code Callable}s to be joined
 * into a single {@code Callable} and inserted into another queue. One reason
 * for doing this is to make a single queue of {@code Callable}s available
 * from multiple threads. E.g., if a task run by an {@link ExecutorService}
 * needs to submit other tasks to the same {@code ExecutorService}, those
 * child tasks ey can be queued and submitted via a {@code QueueJoiner}, while
 * the original task can work on the same queue using a second
 * {@code QueueJoiner}. This prevents the {@code ExecutorService} from
 * deadlocking in the event that the child tasks are unable to make it to the
 * front of the {@code ExecutorService}'s queue, because the thread of the
 * original task will eventually clear the queue on its own.
 *
 * @author Joel Uckelman
 * @since 3.1.11
 */
public class QueueJoiner implements Callable<Void> {
  protected final BlockingQueue<? extends Callable<?>> queue;

  /**
   * Creates a {@link Callable} which calls a {@code Callable} retrieved
   * from a queue.
   *
   * @param queue the queue
   */
  public QueueJoiner(BlockingQueue<? extends Callable<?>> queue) {
    this.queue = queue;
  }

  /**
   * Calls a {@link Callable} from the queue unless the queue is empty.
   *
   * @throws Exception when the {@code Callable} from the queue throws
   */
  public Void call() throws Exception {
    final Callable<?> c = queue.poll();
    if (c != null) c.call();
    return null;
  }
}
