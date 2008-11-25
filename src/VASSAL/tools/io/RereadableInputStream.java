package VASSAL.tools.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.SequenceInputStream;

/**
 *
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class RereadableInputStream extends InputStream { 
  /* 
   * The implementation here is based on the one found at
   * http://www.mattryall.net/blog/2007/03/composition-resettable-stream
   */
  private InputStream src;
  private boolean marked;
  private ByteArrayOutputStream savedBytes;

  public RereadableInputStream(InputStream src) {
    this.src = src;
  }

  @Override
  public boolean markSupported() {
    return true;
  }

  @Override
  public synchronized void mark(int readlimit) {
    savedBytes = new ByteArrayOutputStream(readlimit);
    marked = true;
  }

  @Override
  public synchronized void reset() throws IOException {
    if (!marked)
      throw new IOException("Cannot reset unmarked stream");

    src = new SequenceInputStream(
      new ByteArrayInputStream(savedBytes.toByteArray()), src);

    marked = false;
  }

  @Override 
  public int read() throws IOException {
    final int result = src.read();
    if (marked) savedBytes.write(result);
    return result;
  }

  @Override
  public int read(byte[] b, int off, int len) throws IOException {
    final int count = src.read(b, off, len);
    if (count > 0 && marked) savedBytes.write(b, off, count);
    return count;
  }

  @Override
  public void close() throws IOException {
    src.close();
  }
}
