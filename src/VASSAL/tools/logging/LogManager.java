package VASSAL.tools.logging;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.LinkedBlockingQueue;

import VASSAL.tools.ArrayUtils;

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

  // This thread dispatches log entries to the listeners,
  // so as not to block the generating threads.
  private static Thread thread = null;

  private static final Runnable task = new Runnable() {
    public void run() {
      while (true) {
        try {
          final LogEntry entry = queue.take();
          for (LogListener l : listeners) l.handle(entry);
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
