package VASSAL.tools.logging;

import VASSAL.Info;

public class Logger {
  private Logger() {}

  public static final int MESSAGE = LogEntry.MESSAGE;
  public static final int WARNING = LogEntry.WARNING;
  public static final int ERROR =   LogEntry.ERROR;
  public static final int BUG =     LogEntry.BUG;
  public static final int DEBUG =   LogEntry.DEBUG;
  public static final int SYSTEM =  LogEntry.SYSTEM;

  private static final long pid = Info.getInstanceID();

  public static void log(String message) {
    log(null, message, MESSAGE);
  }

  public static void log(Throwable thrown) {
    log(thrown, null, ERROR);
  }

  public static void log(Throwable thrown, String message) {
    log(thrown, message, ERROR);
  }

  public static void log(String message, int type) {
    log(null, message, type);
  }

  public static void log(Throwable thrown, int type) {
    log(thrown, null, type);
  }

  public static void log(Throwable thrown, String message, int type) {
    LogManager.enqueue(new LogEntry(pid, type, thrown, message));
  }
}
