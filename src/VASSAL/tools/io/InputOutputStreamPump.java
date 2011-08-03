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
import java.io.OutputStream;

import VASSAL.tools.concurrent.listener.DummyEventListener;
import VASSAL.tools.concurrent.listener.EventListener;

/**
 * Pumps an {@link InputStream} to an {@link OutputStream}.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class InputOutputStreamPump implements InputStreamPump,
                                              OutputStreamPump {
  protected InputStream in;
  protected OutputStream out;

  protected volatile boolean running = false;

  protected final EventListener<IOException> ioexListener;

  /**
   * Creates an <code>InputOutputStreamPump</code>.
   */
  public InputOutputStreamPump() {
    this(null, null, new DummyEventListener<IOException>());
  }

  /**
   * Creates an <code>InputOutputStreamPump</code>.
   *
   * @param ioexListener the exception listener
   */
  public InputOutputStreamPump(EventListener<IOException> ioexListener) {
    this(null, null, ioexListener);
  }

  /**
   * Creates an <code>InputOutputStreamPump</code>.
   *
   * @param in the input stream
   * @param out the output stream
   */
  public InputOutputStreamPump(InputStream in, OutputStream out) {
    this(in, out, new DummyEventListener<IOException>());
  }

  /**
   * Creates an <code>InputOutputStreamPump</code>.
   *
   * @param in the input stream
   * @param out the output stream
   * @param ioexListener the exception listener
   */
  public InputOutputStreamPump(InputStream in, OutputStream out,
                               EventListener<IOException> ioexListener) {
    this.in = in;
    this.out = out;
    this.ioexListener = ioexListener;
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
      // Tell someone who cares.
      ioexListener.receive(this, e);
    }
  }
}
