/*
 * $Id$
 *
 * Copyright (c) 2010 by Joel Uckelman
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

package VASSAL.tools.concurrent;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * A thread factory which creates daemon threads. Running daemon threads,
 * unlike regular threads, do not prevent the application from shutting down.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class DaemonThreadFactory implements ThreadFactory {
  protected final AtomicInteger id = new AtomicInteger(0);
  protected final String basename;

  /**
   * Creates a thread factory.
   *
   * @param basename the base name to use for threads created by this factory
   */
  public DaemonThreadFactory(String basename) {
    this.basename = basename;
  }

  /** {@inheritDoc} */
  public Thread newThread(Runnable r) {
    final String name = basename + "-" + id.getAndIncrement();
    final Thread t = new Thread(r, name);
    t.setDaemon(true);
    return t;
  }
}
