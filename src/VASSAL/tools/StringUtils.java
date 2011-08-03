/*
 * $Id$
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

import java.util.Collection;
import java.util.Iterator;

import com.google.common.base.Strings;

/**
 * A collection of methods for manipulating {@link String}s.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 * @deprecated Use {@link org.apache.commons.lang.StringUtils} instead.
 */
@Deprecated
public class StringUtils {
  private StringUtils() {}

  /**
   * Joins an array of{@link Strings} into a single delimiter-separated
   * <code>String</code>.
   *
   * @param delim the delimiter to appear between elements of <code>str</code>
   * @param str the array of <code>String</code>s
   * @return a single delimited <code>String</code>
   */
  public static String join(String delim, String... str) {
    return join(delim, new ArrayIterator<String>(str));
  }

  /** @deprecated Use {@link #join(String,String...)} instead. */
  @Deprecated
  public static String join(String[] str, String delim) {
    return join(delim, str);
  }

  /**
   * Joins an {@link Iterable} of {@link Strings} into a single
   * delimiter-separated <code>String</code>.
   *
   * @param delim the delimiter to appear between elements of <code>str</code>
   * @param str the <code>Iterable</code> of <code>String</code>s
   * @return a single delimited <code>String</code>
   */
  public static String join(String delim, Iterable<String> str) {
    return join(delim, str.iterator());
  }

  /** @deprecated Use {@link #join(Collection<String>,String)} instead. */
  @Deprecated
  public static String join(Collection<String> str, String delim) {
    return join(delim, str);
  }

  /**
   * Joins a {@link Iterator} of {@link Strings} into a single
   * delimiter-separated <code>String</code>.
   *
   * @param delim the delimiter to appear between elements of <code>str</code>
   * @param i the <code>Iterator</code> of <code>String</code>
   * @return a single delimited <code>String</code>
   */
  public static String join(String delim, Iterator<String> i) {
    if (!i.hasNext()) return "";

    final StringBuilder sb = new StringBuilder(i.next());
    while (i.hasNext()) sb.append(delim).append(i.next());

    return sb.toString();
  }
}
