/*
 * $Id$
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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

package VASSAL.tools.opcache;

import java.util.List;

/**
 * An operation with a cachecabe result.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public interface Op<V> {
  /**
   * Runs the <code>Op</code>. This method should be called only by the
   * caching framework.
   *
   * @return the result of running this <code>Op</code>
   */
  public V eval() throws Exception;

  /**
   * Returns a list of <code>Op</code>s on which this <code>Op</code> depends.
   *
   * @return the <code>Op</code>s on which this <code>Op</code> depends
   */
  public List<Op<?>> getSources();

  /**
   * Returns the current cache key for this <code>Op</code>.
   *
   * @return the current cache key for this <code>Op</code>
   */
  public OpCache.Key<V> newKey();

  /**
   * Invalidates the current cache key for this <code>Op</code>.
   */
  public void update();
}
