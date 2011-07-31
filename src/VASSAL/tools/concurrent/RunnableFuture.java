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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools.concurrent;

import java.util.concurrent.Future;

/**
 * A {@link Future} that is {@link Runnable}.
 *
 * This class exists for Java 1.5 compatibility. When we switch to Java 1.6,
 * this class should be removed and replaced by
 * {@link java.util.concurrent.RunnableFuture}.
 *
 *
 * @param <V> the result type returned by this {@code Future}'s
 *            {@code get} method
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public interface RunnableFuture<V> extends Runnable, Future<V> {
  public void run();
}
