/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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
 * Provides static methods for calculating hash codes.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public final class HashCode {
  private HashCode() {}

  public static final int hash(final boolean value) {
    return value ? 1 : 0;
  }

  public static final int hash(final byte value) {
    return (int) value;
  }

  public static final int hash(final char value) {
    return (int) value;
  }

  public static final int hash(final short value) {
    return (int) value;
  }

  public static final int hash(final int value) {
    return value;
  }

  public static final int hash(final long value) {
    return (int)(value ^ (value >>> 32));
  }

  public static final int hash(final float value) {
    return Float.floatToIntBits(value);
  }

  public static final int hash(final double value) {
    long bits = Double.doubleToLongBits(value);
    return (int)(bits ^ (bits >>> 32));
  }

  public static final int hash(final Object value) {
    return value == null ? 0 : value.hashCode();
  }
}
