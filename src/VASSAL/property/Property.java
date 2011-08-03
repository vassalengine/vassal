/*
 * $Id$
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
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

package VASSAL.property;

import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Represents a named property with a specified type.
 *
 * A {@code Property} may optionally specify a default value, to be
 * used when no other value is available.
 *
 * @param <T> the class of the value of this {@code Property}
 * @since 3.2.0
 * @author Joel Uckelman
 */
public final class Property<T> {
  /** The name of this property. */
  public final String name;

  /** The class of the value of this property. */
  public final Class<T> type;

  /** The default value of this property. */
  public final T def;

  private final int hash;

  /**
   * Creates a {@code Property} with {@code null} as its default value.
   *
   * @param name the name of the property
   * @param type the class of the value of the property
   * @throws IllegalArgumentException if {@code name} or {@code type}
   * is {@code null}
   */
  public Property(String name, Class<T> type) {
    this(name, type, null);
  }

  /**
   * Creates a {@code Property}.
   *
   * @param name the name of the property
   * @param type the class of the value of the property
   * @param def  the default value of the property
   * @throws IllegalArgumentException if {@code name} or {@code type}
   * is {@code{ null}
   */
  public Property(String name, Class<T> type, T def) {
    if (name == null) throw new IllegalArgumentException();
    if (type == null) throw new IllegalArgumentException();

    this.name = name;
    this.type = type;
    this.def  = def;

    // Note: The default value for a Property does not take part in
    // equality comparisons, so is not included in the hash code.
    hash = new HashCodeBuilder().append(name).append(type).toHashCode();
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    final Property<?> p = (Property<?>) o;
    return name.equals(p.name) && type.equals(p.type);
  }
}
