/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

package VASSAL.launch;

import static VASSAL.launch.TileProgressPumpStateMachine.DONE;
import static VASSAL.launch.TileProgressPumpStateMachine.INIT;

import java.io.IOException;
import java.io.InputStream;

import VASSAL.tools.concurrent.listener.EventListener;
import VASSAL.tools.image.tilecache.ZipFileImageTiler;
import VASSAL.tools.io.InputStreamPump;

/**
 * A stream pump which receives input from {@link ZipFileImageTiler}.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
class TileProgressPump implements InputStreamPump {

  protected InputStream in;

  protected volatile boolean running = false;

  protected final TileProgressPumpStateMachine sm;
  protected final EventListener<IOException> ioexListener;

  /**
   * Create a <code>TileProgressPump</code>.
   *
   * @param nameListener the listener for new filename events
   * @param progListener the listener for progress events
   * @param ioexListener the listener for {@link IOException}s
   */
  public TileProgressPump(EventListener<String> nameListener,
                          EventListener<Integer> progListener,
                          EventListener<IOException> ioexListener) {
    sm = new TileProgressPumpStateMachine(nameListener, progListener);
    this.ioexListener = ioexListener;
  }

  /** {@inheritDoc} */
  public void setInputStream(InputStream in) {
    if (running) throw new UnsupportedOperationException();

    this.in = in;
  }

  /** {@inheritDoc} */
  public void run() {
    running = true;

    final byte[] buf = new byte[256];
    final StringBuilder sb = new StringBuilder();

    int state = INIT;
    int count;

    try {
      while ((count = in.read(buf)) != -1) {
        state = sm.run(state, buf, 0, count, sb);
      }

      if (state != INIT && state != DONE) {
        throw new IOException("Stream ended before DONE");
      }
    }
    catch (IOException e) {
      // Tell someone who cares.
      ioexListener.receive(this, e);
    }
  }
}
