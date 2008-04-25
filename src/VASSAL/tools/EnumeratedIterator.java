/*
 * $Id$
 *
 * Copyright (c) 2008 Joel Uckelman
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

package VASSAL.tools;

import java.util.Enumeration;
import java.util.Iterator;

/**
 * An adapter for converting {@link Iterator}s into {@link Enumeration}s.
 * Consider altering the code requiring the <code>Enumeration</code> instead
 * of using this class. <code>Enumeration</code>s generally should not be
 * used in new code.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 * @see IterableEnumeration
 */
public class EnumeratedIterator<T> implements Enumeration<T> {
  private final Iterator<T> i;

  /**
   * Creates an <code>EnumeratedIterator</code> from the given
   * <code>Iterator</code>.
   *
   * @param i the <code>Iterator</code> on which this will iterate
   */
  public EnumeratedIterator(Iterator<T> i) {
    this.i = i;
  }

  /** {@inheritDoc} */
  public boolean hasMoreElements() {
    return i.hasNext();
  }

  /** {@inheritDoc} */
  public T nextElement() {
    return i.next();
  }
}
