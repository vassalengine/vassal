package VASSAL.tools.logging;

import java.io.Serializable;

import VASSAL.tools.BugUtils;

public class LogEntry implements Serializable {
  private static final long serialVersionUID = 1L;

  public static final int MESSAGE = 0;
  public static final int WARNING = 1;
  public static final int ERROR = 2;
  public static final int BUG = 3;
  public static final int DEBUG = 4;
  public static final int SYSTEM = 5;

  public final long timestamp;
  public final long pid;
  public final int type;
  public final Throwable thrown;
  public final String message;

  public LogEntry(long pid, int type, Throwable thrown, String message) {
    this(System.currentTimeMillis(), pid, type, thrown, message);
  }

  public LogEntry(long timestamp, long pid, int type,
                  Throwable thrown, String message) {

    if (thrown == null && message == null)
      throw new IllegalArgumentException();

    this.timestamp = timestamp;
    this.pid = pid;
    this.type = type;
    this.thrown = thrown;
    this.message = message;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append(timestamp).append(' ').append(pid);
    if (message != null) sb.append(' ').append(message);
    if (thrown != null) sb.append(BugUtils.getStackTrace(thrown));
    return sb.toString();
  }
}
