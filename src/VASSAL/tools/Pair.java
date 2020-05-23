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

/**
 * An immutable class for pairs of objects.
 *
 * @param <A> the type of the first object
 * @param <B> the type of the second object
 *
 * @author Joel Uckelman
 * @since 3.1.11
 * @deprecated Moved to {@link VASSAL.tools.lang.Pair}.
 */
@Deprecated
public final class Pair<A,B> {
  public final A first;
  public final B second;

  /**
   * Creates a pair with the given values.
   *
   * @param first the first value
   * @param second the second value
   */
  public Pair(A first, B second) {
    this.first = first;
    this.second = second;
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return (first  == null ? 0 : first.hashCode() * 31) +
           (second == null ? 0 : second.hashCode());
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    final Pair<?,?> p = (Pair<?,?>) o;
    return (first  == p.first  || (first  != null && first.equals(p.first))) &&
           (second == p.second || (second != null && second.equals(p.second)));
  }

  /**
   * A convenience method for creating pairs without specifying the types.
   *
   * @param first the first value
   * @param second the second value
   * @return the pair containing {@code first} and (@code second}
   */
  public static <A,B> Pair<A,B> of(A first, B second) {
    return new Pair<A,B>(first, second);
  }
}
