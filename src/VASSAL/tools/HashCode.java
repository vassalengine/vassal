/*
 * $Id$
 *
 * Copyright (c) 2007-2010 by Joel Uckelman
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

import java.util.Arrays;

/**
 * Provides static methods for calculating hash codes.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @deprecated Use {@link org.apache.commons.lang.builder.HashCodeBuilder}
 * instead.
 */
@Deprecated
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
    final long bits = Double.doubleToLongBits(value);
    return (int)(bits ^ (bits >>> 32));
  }

  public static final int hash(final Object value) {
    return value == null ? 0 : value.hashCode();
  }

  public static final int hash(final boolean[] a) {
    return Arrays.hashCode(a);
  }

  public static final int hash(final byte[] a) {
    return Arrays.hashCode(a);
  }

  public static final int hash(final char[] a) {
    return Arrays.hashCode(a);
  }

  public static final int hash(final short[] a) {
    return Arrays.hashCode(a);
  }

  public static final int hash(final int[] a) {
    return Arrays.hashCode(a);
  }

  public static final int hash(final long[] a) {
    return Arrays.hashCode(a);
  }

  public static final int hash(final float[] a) {
    return Arrays.hashCode(a);
  }

  public static final int hash(final double[] a) {
    return Arrays.hashCode(a);
  }

  public static final <T> int hash(final T[] a) {
    return Arrays.hashCode(a);
  }
}
