/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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

package VASSAL.tools;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 *
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */ 
public class ThreadManager {
  private ThreadManager() { }

  private static final BlockingQueue<Runnable> requestQueue =
    new LinkedBlockingQueue<Runnable>();

/*
  private static final BlockingQueue<Runnable> requestQueue =
    new PriorityBlockingQueue<Runnable>(20, new PriorityOrdering());

  private static final class PriorityOrdering implements Comparator<Runnable> {
    public int compare(Runnable x, Runnable y) {
      return
        (x instanceof Prioritizable ? ((Prioritizable) x).getPriority() : 0) -
        (y instanceof Prioritizable ? ((Prioritizable) y).getPriority() : 0);
    }
  }
*/
  
/*
  private static final class Ex extends ThreadPoolExecutor {
    public Ex(int corePoolSize, int maximumPoolSize, long keepAliveTime,
              TimeUnit unit, BlockingQueue<Runnable> workQueue) {
      super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue);
    }

    @Override
    protected <T> RunnableFuture<T> newTaskFor(Runnable runnable, T value) {
      return runnable instanceof RunnableFuture ?
        (RunnableFuture<T>) runnable : super.newTaskFor(runnable, value);
    }
  }
*/

  private static final ExecutorService threadPool =
    new ThreadPoolExecutor(2, 2, 10, TimeUnit.SECONDS, requestQueue);
//    new Ex(2, 2, 10, TimeUnit.SECONDS, requestQueue);

  // FIXME: RunnableFuture does not exist in Java 1.5. Remove this
  // once we switch to 1.6.
  public static <V> Future<V> submit(Runnable task) {
System.out.println("thread queue: " + requestQueue.size());
    return (Future<V>) threadPool.submit(task);
  }

/*
  public static <V> Future<V> submit(RunnableFuture<V> task) {
System.out.println("thread queue: " + requestQueue.size());
    return (Future<V>) threadPool.submit(task);
  }
*/
}
