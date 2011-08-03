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

package VASSAL.tools.logging;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An {@link OutputStream} which feeds an {@link org.slf4j.Logger}.
 * All output is logged at the <code>WARN</code> level.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class LoggedOutputStream extends OutputStream {
  private final Logger logger;

  private final ByteArrayOutputStream buf = new ByteArrayOutputStream();

  /**
   * Creates a <code>LoggedOutputStream</code> with the default logger.
   */
  public LoggedOutputStream() {
    this(LoggerFactory.getLogger(LoggedOutputStream.class));
  }

  /**
   * Creates a <code>LoggedOutputStream</code>.
   *
   * @param logger the logger which receives the output
   */
  public LoggedOutputStream(Logger logger) {
    this.logger = logger;
  }

  /** {@inheritDoc} */
  @Override
  public synchronized void write(int b) {
    // don't write trailing newlines, logger adds those
    if (b == '\n') flush();
    else buf.write(b);
  }

  /** {@inheritDoc} */
  @Override
  public synchronized void write(byte b[], int off, int len) {
    // don't write trailing newlines, logger adds those
    if (b[off+len-1] == '\n') --len;

    buf.write(b, off, len);
    flush();
  }

  /** {@inheritDoc} */
  @Override
  public synchronized void flush() {
    if (buf.size() > 0) {
      logger.warn(new String(buf.toByteArray()));
      buf.reset();
    }
  }
}
