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

package VASSAL.tools;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * An {@link Iterator} for arrays.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class ArrayIterator<T> implements Iterator<T> {

  private T[] array;
  private int pos = 0;

  public ArrayIterator(T... array) {
    this.array = array;
  }

  /** {@inheritDoc} */
  public boolean hasNext() {
    return pos < array.length;
  }

  /** {@inheritDoc} */
  public T next() {
    if (!hasNext()) throw new NoSuchElementException();
    return array[pos++];
  }

  /** {@inheritDoc} */
  public void remove() {
    throw new UnsupportedOperationException();
  }
}
