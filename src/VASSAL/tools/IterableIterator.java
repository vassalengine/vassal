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

import java.util.Iterator;

/**
 * An adapter for converting {@link Iterator}s into {@link Iterable}s.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 * @see IterableEnumeration
 */
public final class IterableIterator<T> implements Iterable<T>, Iterator<T> {
  private final Iterator<T> i;

  /**
   * Creates an <code>IterableIterator</code> from the given
   * <code>Iterator</code>.
   *
   * @param i the <code>Enumeration</code> on which this will iterate
   */
  public IterableIterator(Iterator<T> i) {
    this.i = i;
  }

  /** {@inheritDoc} */
  public Iterator<T> iterator() {
    return i;
  }

  /** {@inheritDoc} */
  public boolean hasNext() {
    return i.hasNext();
  }

  /** {@inheritDoc} */
  public T next() {
    return i.next();
  }

  /** {@inheritDoc} */
  public void remove() {
    i.remove();
  }

  /**
   * A static convenience method for making an <code>Iterator<code>
   * into an <code>Iterable</code>.
   *
   * @param i the <code>Iterator</code>
   * @return an <code>Iterable</code> wrapping <code>i</code>
   */
  public static <T> Iterable<T> iterate(Iterator<T> i) {
    return new IterableIterator<T>(i);
  }
}
