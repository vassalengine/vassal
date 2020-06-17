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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * An rereadable {@link InputStream}. The {@link #reset()} method may be
 * uesd to rewind this input stream to a previously {@link #mark(int)}ed
 * location.
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

  /**
   * Creates a <code>RereadableInputStream</code> which uses the given
   * <code>InputStream</code> as its source.
   *
   * @param src source input stream
   */
  public RereadableInputStream(InputStream src) {
    if (src == null) throw new IllegalArgumentException();
    this.src = src;
  }

  /** {@inheritDoc} */
  @Override
  public boolean markSupported() {
    return true;
  }

  /**
   * Set the current marked position in the stream.
   *
   * Note: The <code>readLimit</code> for this class determines only the
   * initial size of the buffer. Reading more than <code>readLimit</code>
   * bytes after calling this method will not invalidate the previous mark,
   * but will cause the buffer to be resized.
   *
   * @param readLimit the initial buffer size
   */
  @Override
  public synchronized void mark(int readlimit) {
    savedBytes = new ByteArrayOutputStream(readlimit);
    marked = true;
  }

  /** {@inheritDoc} */
  @Override
  public synchronized void reset() throws IOException {
    if (!marked)
      throw new IOException("Cannot reset unmarked stream");

    src = new CompositeInputStream(
      new ByteArrayInputStream(savedBytes.toByteArray()), src);

    marked = false;
  }

  /** {@inheritDoc} */
  @Override
  public int available() throws IOException {
    return src.available();
  }

  /** {@inheritDoc} */
  @Override
  public int read() throws IOException {
    final int result = src.read();
    if (marked) savedBytes.write(result);
    return result;
  }

  /** {@inheritDoc} */
  @Override
  public int read(byte[] b, int off, int len) throws IOException {
    final int count = src.read(b, off, len);
    if (count > 0 && marked) savedBytes.write(b, off, count);
    return count;
  }

  /** {@inheritDoc} */
  @Override
  public void close() throws IOException {
    src.close();
  }
}
