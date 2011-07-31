/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman
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

package VASSAL.tools.swing;

import VASSAL.tools.concurrent.SimpleRunnableFuture;

/**
 * A {@link RunnableFuture} for use on the Event Dispatch Thread.
 *
 * @param <V> the result type returned by the {@see #get} method
 * @author Joel Uckelman
 * @since 3.2.0
 */
public abstract class EDTRunnableFuture<V> extends SimpleRunnableFuture<V> {
  /**
   * Creates a {@link RunnableFuture} which will be run on the Event Dispatch
   * Thread.
   */
  public EDTRunnableFuture() { }

  /**
   * Creates a {@link RunnableFuture} which will be run on the Event Dispatch
   * Thread.
   *
   * @param result the result to return
   */
  public EDTRunnableFuture(V result) {
    this.result = result;
  }

  /**
   * This method is run on the EDT.
   *
   * @throws Exception any exception
   */
  protected abstract void runOnEDT() throws Exception;

  /** {@inheritDoc} */
  public final void run() {
    try {
      runOnEDT();
      set(result);
    }
    catch (Throwable t) {
      setException(t);
    }
  }
}
