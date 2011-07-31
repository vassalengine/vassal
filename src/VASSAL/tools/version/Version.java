/*
 * $Id$
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

package VASSAL.tools.version;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * A dotted-integer version, pre-parsed for easy comparison.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 * @see VersionTokenizer
 * @see VassalVersion
 */
public class Version implements Comparable<Version> {
  protected final List<Integer> tokens = new ArrayList<Integer>();
  protected final String vstring;
  protected final boolean valid;

  /**
   * A dotted-integer version.
   *
   * @param v a version string
   */
  public Version(String v) {
    this(v, new SimpleVersionTokenizer(v));
  }

  protected Version(String v, VersionTokenizer tok) {
    vstring = v;

    boolean parsed;
    try {
      while (tok.hasNext()) tokens.add(tok.next());
      parsed = true;
    }
    catch (VersionFormatException e) {
      parsed = false;
    }

    valid = parsed;
  }

  public boolean isValid() {
    return valid;
  }

  /**
   * Compares dotted-integer version strings.
   *
   * @return negative if {@code this < v}, positive if {@code this > v},
   * and zero if {@code this == v} or if the parseable parts of the
   * versions are equal.
   */
  public int compareTo(Version v) {
    final Iterator<Integer> i = this.tokens.iterator();
    final Iterator<Integer> j = v.tokens.iterator();

    // find the first token where this and v differ
    while (i.hasNext() && j.hasNext()) {
      final int a = i.next();
      final int b = j.next();

      if (a != b) return a - b;
    }

    // versions which match up to the point of invalidity are equal
    if (!this.isValid() || !v.isValid()) return 0;

    // otherwise, the shorter one is earlier; or they're the same
    return i.hasNext() ? 1 : (j.hasNext() ? -1 : 0);
  }

  public String toString() {
    return vstring;
  }
}
