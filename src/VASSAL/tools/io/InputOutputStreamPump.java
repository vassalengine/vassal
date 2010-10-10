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
public class InputOutputStreamPump implements InputStreamPump,
                                              OutputStreamPump {
  private static final Logger logger =
    LoggerFactory.getLogger(InputOutputStreamPump.class);

  protected InputStream in;
  protected OutputStream out;

  protected volatile boolean running = false;

  public InputOutputStreamPump() {}

  /**
   * Creates a <code>StreamPump</code>.
   *
   * @param in the input stream
   * @param out the output stream
   */
  public InputOutputStreamPump(InputStream in, OutputStream out) {
    this.in = in;
    this.out = out;
  }

  /**
   * Sets the input stream.
   *
   * @param in the input stream
   * @throws UnsupportedOperationException if called after the pump is started
   */
  public void setInputStream(InputStream in) {
    if (running) throw new UnsupportedOperationException();

    this.in = in;
  }

  /**
   * Sets the output stream.
   *
   * @param out the output stream
   * @throws UnsupportedOperationException if called after the pump is started
   */
  public void setOutputStream(OutputStream out) {
    if (running) throw new UnsupportedOperationException();

    this.out = out;
  }

  /** {@inheritDoc} */
  public void run() {
    running = true;

    try {
      IOUtils.copy(in, out);
    }
    catch (IOException e) {
// FIXME: there should be an exception listener
      logger.error("", e);
    }
  }
}
