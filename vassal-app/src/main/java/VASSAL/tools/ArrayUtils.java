/*
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

import java.lang.reflect.Array;
import java.util.Arrays;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 * @deprecated Use {@link org.apache.commons.lang3.ArrayUtils}.
 */
@Deprecated(since = "2021-12-01", forRemoval = true)
public class ArrayUtils {
  private ArrayUtils() {}

  public static boolean[] prepend(boolean[] orig, boolean e) {
    final boolean[] tmp = new boolean[orig.length + 1];
    tmp[0] = e;
    System.arraycopy(orig, 0, tmp, 1, orig.length);
    return tmp;
  }

  public static byte[] prepend(byte[] orig, byte e) {
    final byte[] tmp = new byte[orig.length + 1];
    tmp[0] = e;
    System.arraycopy(orig, 0, tmp, 1, orig.length);
    return tmp;
  }

  public static char[] prepend(char[] orig, char e) {
    final char[] tmp = new char[orig.length + 1];
    tmp[0] = e;
    System.arraycopy(orig, 0, tmp, 1, orig.length);
    return tmp;
  }

  public static double[] prepend(double[] orig, double e) {
    final double[] tmp = new double[orig.length + 1];
    tmp[0] = e;
    System.arraycopy(orig, 0, tmp, 1, orig.length);
    return tmp;
  }

  public static float[] prepend(float[] orig, float e) {
    final float[] tmp = new float[orig.length + 1];
    tmp[0] = e;
    System.arraycopy(orig, 0, tmp, 1, orig.length);
    return tmp;
  }

  public static int[] prepend(int[] orig, int e) {
    final int[] tmp = new int[orig.length + 1];
    tmp[0] = e;
    System.arraycopy(orig, 0, tmp, 1, orig.length);
    return tmp;
  }

  public static long[] prepend(long[] orig, long e) {
    final long[] tmp = new long[orig.length + 1];
    tmp[0] = e;
    System.arraycopy(orig, 0, tmp, 1, orig.length);
    return tmp;
  }

  public static short[] prepend(short[] orig, short e) {
    final short[] tmp = new short[orig.length + 1];
    tmp[0] = e;
    System.arraycopy(orig, 0, tmp, 1, orig.length);
    return tmp;
  }

  @SuppressWarnings("unchecked")
  public static <T> T[] prepend(T[] orig, T e) {
    return prepend((Class<T[]>) orig.getClass(), orig, e);
  }

  @SuppressWarnings("unchecked")
  public static <T, X extends T, Y extends T> T[] prepend(Class<T[]> type,
                                                        X[] orig, Y e) {
    final T[] tmp =
      (T[]) Array.newInstance(type.getComponentType(), orig.length + 1);
    tmp[0] = e;
    System.arraycopy(orig, 0, tmp, 1, orig.length);
    return tmp;
  }

  public static boolean[] append(boolean[] orig, boolean e) {
    final boolean[] tmp = Arrays.copyOf(orig, orig.length + 1);
    tmp[orig.length] = e;
    return tmp;
  }

  public static byte[] append(byte[] orig, byte e) {
    final byte[] tmp = Arrays.copyOf(orig, orig.length + 1);
    tmp[orig.length] = e;
    return tmp;
  }

  public static char[] append(char[] orig, char e) {
    final char[] tmp = Arrays.copyOf(orig, orig.length + 1);
    tmp[orig.length] = e;
    return tmp;
  }

  public static double[] append(double[] orig, double e) {
    final double[] tmp = Arrays.copyOf(orig, orig.length + 1);
    tmp[orig.length] = e;
    return tmp;
  }

  public static float[] append(float[] orig, float e) {
    final float[] tmp = Arrays.copyOf(orig, orig.length + 1);
    tmp[orig.length] = e;
    return tmp;
  }

  public static int[] append(int[] orig, int e) {
    final int[] tmp = Arrays.copyOf(orig, orig.length + 1);
    tmp[orig.length] = e;
    return tmp;
  }

  public static long[] append(long[] orig, long e) {
    final long[] tmp = Arrays.copyOf(orig, orig.length + 1);
    tmp[orig.length] = e;
    return tmp;
  }

  public static short[] append(short[] orig, short e) {
    final short[] tmp = Arrays.copyOf(orig, orig.length + 1);
    tmp[orig.length] = e;
    return tmp;
  }

  @SuppressWarnings("unchecked")
  public static <T> T[] append(T[] orig, T e) {
    return append((Class<T[]>) orig.getClass(), orig, e);
  }

  public static <T, X extends T, Y extends T> T[] append(Class<T[]> type,
                                                       X[] orig, Y e) {
    final T[] tmp = Arrays.copyOf(orig, orig.length + 1, type);
    tmp[orig.length] = e;
    return tmp;
  }

  public static boolean[] append(boolean[] a, boolean... b) {
    final boolean[] tmp = new boolean[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, a.length);
    System.arraycopy(b, 0, tmp, a.length, b.length);
    return tmp;
  }

  public static byte[] append(byte[] a, byte... b) {
    final byte[] tmp = new byte[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, a.length);
    System.arraycopy(b, 0, tmp, a.length, b.length);
    return tmp;
  }

  public static char[] append(char[] a, char... b) {
    final char[] tmp = new char[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, a.length);
    System.arraycopy(b, 0, tmp, a.length, b.length);
    return tmp;
  }

  public static double[] append(double[] a, double... b) {
    final double[] tmp = new double[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, a.length);
    System.arraycopy(b, 0, tmp, a.length, b.length);
    return tmp;
  }

  public static float[] append(float[] a, float... b) {
    final float[] tmp = new float[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, a.length);
    System.arraycopy(b, 0, tmp, a.length, b.length);
    return tmp;
  }

  public static int[] append(int[] a, int... b) {
    final int[] tmp = new int[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, a.length);
    System.arraycopy(b, 0, tmp, a.length, b.length);
    return tmp;
  }

  public static long[] append(long[] a, long... b) {
    final long[] tmp = new long[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, a.length);
    System.arraycopy(b, 0, tmp, a.length, b.length);
    return tmp;
  }

  public static short[] append(short[] a, short... b) {
    final short[] tmp = new short[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, a.length);
    System.arraycopy(b, 0, tmp, a.length, b.length);
    return tmp;
  }

  @SuppressWarnings("unchecked")
  public static <T> T[] append(T[] a, T... b) {
    return append((Class<T[]>) a.getClass(), a, b);
  }

  @SuppressWarnings("unchecked")
  public static <T, X extends T, Y extends T> T[] append(Class<T[]> type,
                                                       X[] a, Y... b) {
    final T[] tmp = (T[]) Array.newInstance(type.getComponentType(),
                                            a.length + b.length);
    System.arraycopy(a, 0, tmp, 0, a.length);
    System.arraycopy(b, 0, tmp, a.length, b.length);
    return tmp;
  }

  public static boolean[] insert(boolean[] orig, int pos, boolean e) {
    final boolean[] tmp = new boolean[orig.length + 1];
    System.arraycopy(orig, 0, tmp, 0, pos);
    tmp[pos] = e;
    System.arraycopy(orig, pos, tmp, pos, orig.length - pos);
    return tmp;
  }

  public static byte[] insert(byte[] orig, int pos, byte e) {
    final byte[] tmp = new byte[orig.length + 1];
    System.arraycopy(orig, 0, tmp, 0, pos);
    tmp[pos] = e;
    System.arraycopy(orig, pos, tmp, pos, orig.length - pos);
    return tmp;
  }

  public static char[] insert(char[] orig, int pos, char e) {
    final char[] tmp = new char[orig.length + 1];
    System.arraycopy(orig, 0, tmp, 0, pos);
    tmp[pos] = e;
    System.arraycopy(orig, pos, tmp, pos, orig.length - pos);
    return tmp;
  }

  public static double[] insert(double[] orig, int pos, double e) {
    final double[] tmp = new double[orig.length + 1];
    System.arraycopy(orig, 0, tmp, 0, pos);
    tmp[pos] = e;
    System.arraycopy(orig, pos, tmp, pos, orig.length - pos);
    return tmp;
  }

  public static float[] insert(float[] orig, int pos, float e) {
    final float[] tmp = new float[orig.length + 1];
    System.arraycopy(orig, 0, tmp, 0, pos);
    tmp[pos] = e;
    System.arraycopy(orig, pos, tmp, pos, orig.length - pos);
    return tmp;
  }

  public static int[] insert(int[] orig, int pos, int e) {
    final int[] tmp = new int[orig.length + 1];
    System.arraycopy(orig, 0, tmp, 0, pos);
    tmp[pos] = e;
    System.arraycopy(orig, pos, tmp, pos, orig.length - pos);
    return tmp;
  }

  public static long[] insert(long[] orig, int pos, long e) {
    final long[] tmp = new long[orig.length + 1];
    System.arraycopy(orig, 0, tmp, 0, pos);
    tmp[pos] = e;
    System.arraycopy(orig, pos, tmp, pos, orig.length - pos);
    return tmp;
  }

  public static short[] insert(short[] orig, int pos, short e) {
    final short[] tmp = new short[orig.length + 1];
    System.arraycopy(orig, 0, tmp, 0, pos);
    tmp[pos] = e;
    System.arraycopy(orig, pos, tmp, pos, orig.length - pos);
    return tmp;
  }

  @SuppressWarnings("unchecked")
  public static <T> T[] insert(T[] orig, int pos, T e) {
    return insert((Class<T[]>) orig.getClass(), orig, pos, e);
  }

  @SuppressWarnings("unchecked")
  public static <T, X extends T, Y extends T> T[] insert(Class<T[]> type,
                                                       X[] orig, int pos, Y e) {
    final T[] tmp =
      (T[]) Array.newInstance(type.getComponentType(), orig.length + 1);
    System.arraycopy(orig, 0, tmp, 0, pos);
    tmp[pos] = e;
    System.arraycopy(orig, pos, tmp, pos + 1, orig.length - pos);
    return tmp;
  }

  public static boolean[] insert(boolean[] a, int pos, boolean... b) {
    final boolean[] tmp = new boolean[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, pos);
    System.arraycopy(b, 0, tmp, pos, b.length);
    System.arraycopy(a, pos, tmp, pos + b.length, a.length - pos);
    return tmp;
  }

  public static byte[] insert(byte[] a, int pos, byte... b) {
    final byte[] tmp = new byte[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, pos);
    System.arraycopy(b, 0, tmp, pos, b.length);
    System.arraycopy(a, pos, tmp, pos + b.length, a.length - pos);
    return tmp;
  }

  public static char[] insert(char[] a, int pos, char... b) {
    final char[] tmp = new char[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, pos);
    System.arraycopy(b, 0, tmp, pos, b.length);
    System.arraycopy(a, pos, tmp, pos + b.length, a.length - pos);
    return tmp;
  }

  public static double[] insert(double[] a, int pos, double... b) {
    final double[] tmp = new double[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, pos);
    System.arraycopy(b, 0, tmp, pos, b.length);
    System.arraycopy(a, pos, tmp, pos + b.length, a.length - pos);
    return tmp;
  }

  public static float[] insert(float[] a, int pos, float... b) {
    final float[] tmp = new float[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, pos);
    System.arraycopy(b, 0, tmp, pos, b.length);
    System.arraycopy(a, pos, tmp, pos + b.length, a.length - pos);
    return tmp;
  }

  public static int[] insert(int[] a, int pos, int... b) {
    final int[] tmp = new int[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, pos);
    System.arraycopy(b, 0, tmp, pos, b.length);
    System.arraycopy(a, pos, tmp, pos + b.length, a.length - pos);
    return tmp;
  }

  public static long[] insert(long[] a, int pos, long... b) {
    final long[] tmp = new long[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, pos);
    System.arraycopy(b, 0, tmp, pos, b.length);
    System.arraycopy(a, pos, tmp, pos + b.length, a.length - pos);
    return tmp;
  }

  public static short[] insert(short[] a, int pos, short... b) {
    final short[] tmp = new short[a.length + b.length];
    System.arraycopy(a, 0, tmp, 0, pos);
    System.arraycopy(b, 0, tmp, pos, b.length);
    System.arraycopy(a, pos, tmp, pos + b.length, a.length - pos);
    return tmp;
  }

  @SuppressWarnings("unchecked")
  public static <T> T[] insert(T[] a, int pos, T... b) {
    return insert((Class<T[]>) a.getClass(), a, pos, b);
  }

  @SuppressWarnings("unchecked")
  public static <T, X extends T, Y extends T> T[] insert(Class<T[]> type,
                                                       X[] a, int pos, Y... b) {
    final T[] tmp =
      (T[]) Array.newInstance(type.getComponentType(), a.length + b.length);
    System.arraycopy(a, 0, tmp, 0, pos);
    System.arraycopy(b, 0, tmp, pos, b.length);
    System.arraycopy(a, pos, tmp, pos + b.length, a.length - pos);
    return tmp;
  }

  public static float[] remove(float[] orig, float e) {
    for (int i = 0; i < orig.length; i++) {
      if (orig[i] == e) {
        final float[] tmp = new float[orig.length - 1];
        System.arraycopy(orig, 0, tmp, 0, i);
        if (i < tmp.length)
          System.arraycopy(orig, i + 1, tmp, i, orig.length - i - 1);
        return tmp;
      }
    }
    return orig;
  }

  public static int[] remove(int[] orig, int e) {
    for (int i = 0; i < orig.length; i++) {
      if (orig[i] == e) {
        final int[] tmp = new int[orig.length - 1];
        System.arraycopy(orig, 0, tmp, 0, i);
        if (i < tmp.length)
          System.arraycopy(orig, i + 1, tmp, i, orig.length - i - 1);
        return tmp;
      }
    }
    return orig;
  }

  public static long[] remove(long[] orig, long e) {
    for (int i = 0; i < orig.length; i++) {
      if (orig[i] == e) {
        final long[] tmp = new long[orig.length - 1];
        System.arraycopy(orig, 0, tmp, 0, i);
        if (i < tmp.length)
          System.arraycopy(orig, i + 1, tmp, i, orig.length - i - 1);
        return tmp;
      }
    }
    return orig;
  }

  public static short[] remove(short[] orig, short e) {
    for (int i = 0; i < orig.length; i++) {
      if (orig[i] == e) {
        final short[] tmp = new short[orig.length - 1];
        System.arraycopy(orig, 0, tmp, 0, i);
        if (i < tmp.length)
          System.arraycopy(orig, i + 1, tmp, i, orig.length - i - 1);
        return tmp;
      }
    }
    return orig;
  }

  @SuppressWarnings("unchecked")
  public static <T> T[] remove(T[] orig, T e) {
    for (int i = 0; i < orig.length; i++) {
      if (orig[i].equals(e)) {
        final T[] tmp = (T[]) Array.newInstance(
          orig.getClass().getComponentType(), orig.length - 1);
        System.arraycopy(orig, 0, tmp, 0, i);
        if (i < tmp.length)
          System.arraycopy(orig, i + 1, tmp, i, orig.length - i - 1);
        return tmp;
      }
    }
    return orig;
  }
}
