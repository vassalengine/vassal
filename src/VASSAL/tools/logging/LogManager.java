package VASSAL.tools.logging;

import java.util.concurrent.BlockingQueue;
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
          synchronized (LogManager.class) {
            for (LogListener l : listeners) l.handle(entry);
          }
        }
        catch (InterruptedException e) {
          // What do to here????
        }
      }
    }
  };

  private static LogListener[] listeners = new LogListener[0];

  public static synchronized void addLogListener(LogListener l) {
    listeners = ArrayUtils.append(listeners, l);
  }

  public static synchronized void removeLogListener(LogListener l) {
    listeners = ArrayUtils.remove(listeners, l);
  }

  public static synchronized LogListener[] getLogListeners() {
    return ArrayUtils.copyOf(listeners);
  }
}
