/*
 * $Id$
 *
 * Copyright (c) 2007 Joel Uckelman
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
 * An adapter for converting {@link Enumeration}s into {@link Iterable}s.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 * @see IterableIterator
 */
public final class IterableEnumeration<T> implements Iterable<T>, Iterator<T> {
  private final Enumeration<T> e;

  /**
   * Creates an <code>IterableEnumeration</code> from the given
   * <code>Enumeration</code>.
   *
   * @param e the <code>Enumeration</code> on which this will iterate
   */
  public IterableEnumeration(Enumeration<T> e) {
    this.e = e;
  }

  /** {@inheritDoc} */
  public Iterator<T> iterator() {
    return this;
  }

  /** {@inheritDoc} */
  public boolean hasNext() {
    return e.hasMoreElements();
  }

  /** {@inheritDoc} */
  public T next() {
    return e.nextElement();
  }

  /**
   * {@inheritDoc}
   *
   * @throws UnsupportedOperationException becuase <code>Enumeration</code>
   * do not permit removal of elements.
   */
  public void remove() {
    throw new UnsupportedOperationException();
  }

  /**
   * A static convenience method for making an <code>Enumeration<code>
   * into an <code>Iterable</code>.
   *
   * @param e the <code>Enumeration</code>
   * @return an <code>Iterable</code> wrapping <code>e</code>
   */
  public static <T> Iterable<T> iterate(Enumeration<T> e) {
    return new IterableEnumeration<T>(e);
  }
}
