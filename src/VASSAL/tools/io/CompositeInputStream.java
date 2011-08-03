/*
 * $Id$
 *
 * Copyright (c) 2008-2010 by Joel Uckelman
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
package VASSAL.tools.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.SequenceInputStream;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

/**
 * An {@link InputStream} which concatenates other <code>InputStreams</code>.
 * As with {@link SequenceInputStream}, the first stream is read until EOF,
 * followed by the second, and so on, until all input streams are exhausted.
 *
 * Note: {@link SequenceInputStream#available()} returns only the number of
 * bytes available from the current stream in the sequence, which makes it
 * difficult to efficiently allocate a buffer into which to read the bytes.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 * @see SequenceInputStream
 */
public class CompositeInputStream extends InputStream {

  protected final LinkedList<InputStream> queue;

  protected InputStream in;

  /**
   * Creates a <code>CompositeInputStream</code> from the given sequence
   * of <code>InputStream</code>s.
   *
   * @param streams the <code>InputStream</code>s to be concatenated
   */
  public CompositeInputStream(List<InputStream> streams) {
    queue = new LinkedList<InputStream>(streams);
    in = queue.poll();
  }

  /**
   * Creates a <code>CompositeInputStream</code> from the given sequence
   * of <code>InputStream</code>s.
   *
   * @param streams the <code>InputStream</code>s to be concatenated
   */
  public CompositeInputStream(InputStream... streams) {
    this(Arrays.asList(streams));
  }

  protected void nextStream() throws IOException {
    if (in != null) {
      in.close();
      in = queue.poll();
    }
  }

  /** {@inheritDoc} */
  @Override
  public int available() throws IOException {
    int bytes = in != null ? in.available() : 0;
    for (InputStream ch : queue) {
      bytes += Math.max(ch.available(), 0);
    }
    return bytes;
  }

  /** {@inheritDoc} */
  @Override
  public int read() throws IOException {
    if (in == null) return -1;

    final int c = in.read();
    if (c != -1) return c;

    nextStream();
    return read();
  }

  /** {@inheritDoc} */
  @Override
  public int read(byte b[], int off, int len) throws IOException {
    if (in == null) return -1;
    if (len == 0) return 0;

    final int count = in.read(b, off, len);
    if (count > 0) return count;

    nextStream();
    return read(b, off, len);
  }

  /** {@inheritDoc} */
  @Override
  public void close() throws IOException {
    while (in != null) nextStream();
  }
}
