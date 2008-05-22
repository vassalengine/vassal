/*
 * $Id: ImageFileFilter.java 3562 2008-05-06 12:46:57Z uckelman $
 *
 * Copyright (c) 2008 by Joel Uckelman
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

/**
 * A collection of methods for manipulating {@link String}s.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class StringUtils {
  private StringUtils() {}

  /**
   * Joins an array of{@link Strings} into a single delimiter-separated
   * <code>String</code>.
   *
   * @param str the array of <code>String</code>s
   * @param delim the delimiter to appear between elements of <code>str</code>
   * @return a single delimited <code>String</code>
   */
  public static String join(String[] str, String delim) {
    if (str == null) return null;
    if (str.length == 0) return "";

    final StringBuilder sb = new StringBuilder(str[0]);
    for (int i = 1; i < str.length; i++) {
      sb.append(delim).append(str[i]);
    }

    return sb.toString();
  }

  /**
   * Joins a {@link Collection} of {@link Strings} into a single
   * delimiter-separated <code>String</code>.
   *
   * @param str the <code>Collection</code> of <code>String</code>s
   * @param delim the delimiter to appear between elements of <code>str</code>
   * @return a single delimited <code>String</code>
   */
  public static String join(Collection<String> str, String delim) {
    if (str == null) return null;
    if (str.size() == 0) return "";

    final Iterator<String> i = str.iterator();
    final StringBuilder sb = new StringBuilder(i.next());
    
    while (i.hasNext()) sb.append(delim).append(i.next());

    return sb.toString();
  }
}
