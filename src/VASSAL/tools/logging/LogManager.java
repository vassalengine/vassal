/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.logging;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class LogManager {
  private static final BlockingQueue<LogEntry> queue =
    new LinkedBlockingQueue<LogEntry>();

  public static void enqueue(LogEntry entry) {
    queue.add(entry);
  }

  public static void start() {
    if (thread == null) {
      thread = new Thread(task);
      thread.setPriority(Thread.MIN_PRIORITY);
      thread.setDaemon(true);
      thread.start();
    }
  }

  private static class Flusher extends LogEntry {
    private static final long serialVersionUID = 1L;

    public final CountDownLatch latch = new CountDownLatch(1);

    public Flusher() {
      super(0L, 0, null, "");
    }
  }
 
  public static void flush() {
    final Flusher f = new Flusher();
    queue.add(f);

    try {
      f.latch.await();
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  // This thread dispatches log entries to the listeners,
  // so as not to block the generating threads.
  private static Thread thread = null;

  private static final Runnable task = new Runnable() {
    public void run() {
      while (true) {
        try {
          final LogEntry entry = queue.take();
          if (entry instanceof Flusher) {
            for (LogListener l : listeners) l.flush();
            ((Flusher) entry).latch.countDown();
          }
          else {
            for (LogListener l : listeners) l.handle(entry);
          }
        }
        catch (InterruptedException e) {
          // FIXME: What do to here????
        }
      }
    }
  };

  private static CopyOnWriteArrayList<LogListener> listeners =
    new CopyOnWriteArrayList<LogListener>();

  public static void addLogListener(LogListener l) {
    listeners.add(l);
  }

  public static void removeLogListener(LogListener l) {
    listeners.remove(l);
  }

  public static LogListener[] getLogListeners() {
    return listeners.toArray(new LogListener[0]);
  }
}
