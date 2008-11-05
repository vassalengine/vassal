package VASSAL.tools.logging;

import java.io.IOException;
import java.io.OutputStream;

public class LogOutputStreamAdapter implements LogListener {
  private final OutputStream out;

  public LogOutputStreamAdapter(OutputStream out) {
    if (out == null) throw new NullPointerException();
    this.out = out;
  }

  public void handle(LogEntry entry) {
    try {
      out.write(entry.toString().getBytes());
      out.write('\n');
    }
    catch (IOException e) {
      // FIXME: What to do here????
    }
  }
}
