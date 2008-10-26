package VASSAL.tools.logging;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;

public class LoggedOutputStream extends OutputStream {
  private final long pid;

  private final ByteArrayOutputStream buf = new ByteArrayOutputStream();

  public LoggedOutputStream(long pid) {
    this.pid = pid;
  }

  public synchronized void write(int b) {
    buf.write(b);
    if (b == '\n') flush(); 
  }

  public synchronized void write(byte b[], int off, int len) {
    flush();
    LogManager.enqueue(
      new LogEntry(pid, LogEntry.SYSTEM, null, new String(b, off, len)));
  }

  public synchronized void flush() {
    if (buf.size() > 0) {
      LogManager.enqueue(new LogEntry(pid, LogEntry.SYSTEM,
                                      null, new String(buf.toByteArray())));
      buf.reset();
    }
  } 
} 
