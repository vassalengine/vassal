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

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 * A {@link Future} which provides methods for setting its result.
 *
 * @since author Joel Uckelman
 * @since 3.2.0
 */
public interface SettableFuture<V> extends Future<V> {
  /**
   * Sets the exception to be wrapped in an {@link ExecutionException} and
   * thrown by {@link #get}.
   *
   * <b>May be called only from the thread executing the computation.</b>
   *
   * @param t the exception
   */
  public void setException(Throwable t);

  /**
   * Sets the result to be returned by {@link #get}.
   *
   * <b>May be called only from the thread executing the computation.</b>
   *
   * @param r the result
   */
  public void set(V r);
}
