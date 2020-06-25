/*
 * Copyright (c) 2007-2008 by Joel Uckelman
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;

/**
 * A {@link ByteArrayOutputStream} which does not retain its byte buffer,
 * and can produce an {@link InputStream} sharing the same byte buffer.
 * The advantage of this is that the byte buffer can be shared between the
 * output and input, and provides better performance when the stream
 * will be used only a single time and the buffer is full when
 * {@link #toByteArray()} is called.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class FastByteArrayOutputStream extends ByteArrayOutputStream {
  /**
   * Creates a new byte array output stream. The buffer capacity is initially
   * 32 bytes.
   */
  public FastByteArrayOutputStream() {
    super();
  }

  /**
   * Creates a new byte array output stream, with the specified buffer
   * capacity, in bytes.
   *
   * @param size the initial size
   */
  public FastByteArrayOutputStream(int size) {
    super(size);
  }

  /**
   * Creates a newly allocated byte array. Its size is the current size of
   * this output stream. If the buffer is full, then the array returned is
   * the buffer itself, not a copy, and a new empty buffer is created.
   * Otherwise, a copy of valid portion of the buffer is returned.
   *
   * @return the current contents of this output stream as a byte array
   */
  @Override
  public byte[] toByteArray() {
    if (count == buf.length) {
      count = 0;
      final byte[] ret = buf;
      buf = new byte[0];
      return ret;
    }
    else {
      return super.toByteArray();
    }
  }

  /**
   * Creates an <code>InputStream</code> which reads the bytes stored by
   * this <code>FastByteArrayOutputStream</code>.
   *
   * @return the <code>InputStream</code> for this buffer
   */
  public InputStream toInputStream() {
    return new ByteArrayInputStream(buf, 0, count);
  }
}
