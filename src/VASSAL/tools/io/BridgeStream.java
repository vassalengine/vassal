/*
 * Copyright (c) 2007 by Joel Uckelman
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
 * A {@link ByteArrayOutputStream} which can produce an {@link InputStream}
 * sharing the same byte buffer. The advantage of this is that the byte
 * buffer can be shared between the output and input.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class BridgeStream extends ByteArrayOutputStream {

  /**
   * Creates a <code>BridgeStream</code> with the default initial buffer
   * size.
   */
  public BridgeStream() {
    super();
  }

  /**
   * Creates a <code>BridgeStream</code> with the given initial buffer size.
   *
   * @param size the initial buffer size, in bytes
   */
  public BridgeStream(int size) {
    super(size);
  }

  /**
   * Creates an <code>InputStream</code> which reads the bytes stored by
   * this <code>BridgeStream</code>.
   * 
   * @return the <code>InputStream</code> for this buffer
   */
  public InputStream toInputStream() {
    return new ByteArrayInputStream(buf, 0, count);
  }
}
