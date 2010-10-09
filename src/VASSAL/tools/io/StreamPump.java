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

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Pumps an {@link InputStream} to an {@link OutputStream}.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class StreamPump implements Runnable {
  private static final Logger logger =
    LoggerFactory.getLogger(StreamPump.class);

  protected final InputStream in;
  protected final OutputStream out;

  /**
   * Creates a <code>StreamPump</code>.
   *
   * @param in the input stream
   * @param out the output stream
   */
  public StreamPump(InputStream in, OutputStream out) {
    if (in == null) throw new IllegalArgumentException("in == null");
    if (out == null) throw new IllegalArgumentException("out == null");

    this.in = in;
    this.out = out;
  }

  /** {@inheritDoc} */
  public void run() {
    try {
      IOUtils.copy(in, out);
    }
    catch (IOException e) {
// FIXME: there should be an exception listener
      logger.error("", e);
    }
  }
}
