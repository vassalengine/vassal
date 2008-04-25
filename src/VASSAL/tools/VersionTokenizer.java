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

import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A finite-state machine for converting VASSAL version numbers into
 * a series of integers. The integers thus returned from two different
 * tokenizers may be compared to determine the temporal ordering of two
 * VASSAL versions. Valid version strings are matched by the following
 * regular expression: <code>\d+(.\d+)*(-(svn|beta|rc)\d+)?</code>. Old
 * version numbers which are not valid by current standards (e.g., 3.0b6)
 * may be successfully parsed far enough to determine their ordering with
 * respect to post-3.1.0 versions.
 *
 * <p>Nonnumeric parts of a version string are tokenized as follows:</p>
 * <table>
 *  <tr><td><code>svn</code></td><td>0</td></tr>
 *  <tr><td><code>beta</code></td><td>1</td></tr>
 *  <tr><td><code>rc</code></td><td>2</td></tr>
 *  <tr><td><code>-</code> (tag delimiter)</td><td>-2</td></tr>
 *  <tr><td>end-of-string</td><td>-1</td></tr>
 * </table>
 * <p>This mapping ensures that of two version strings with the same
 * version number, if one has a tag (the part starting with the hyphen) but
 * the other does not, then one with the tag will have a lexically smaller
 * token stream than the one without. E.g., 3.1.0-svn2708 &lt; 3.1.0,
 * since the two token streams are <code>3 1 0 -2 2708 -1</code> and
 * <code>3 1 0 -1</code>, respectively.</p>
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @see Info.compareVersions(String,String)
 */
public class VersionTokenizer {
  private String v;
  private int state = 0;

  /**
   * Constructs a <code>VersionTokenizer</code> which operates on a
   * version <code>String</code>.
   *
   * @param version the version <code>String</code> to parse
   * @throws IllegalArgumentException if <code>version == null</code>.
   */  
  public VersionTokenizer(String version) {
    if (version == null) throw new IllegalArgumentException();
    v = version;
  }

  /**
   * Returns <code>true</code> if the version <code>String</code> is
   * not fully parsed.
   *
   * @return <code>true</code> if {@link #next()} will return more
   * integers
   */
  public boolean hasNext() {
    return v.length() > 0 || state == 4;
  }

  /**
   * Returns an integer representing the next token.
   *
   * @return the integer representing the next token
   * @throws VersionFormatException if the string deviates from
   * the current version formatting rules at the next token.
   * @throws NoSuchElementException if this method is called when
   * {@link hasNext()} would return <code>false</code>.
   */
  public int next() throws VersionFormatException {
    if (!hasNext()) throw new NoSuchElementException(); 

    int n;

    while (true) {
      switch (state) {
      case 0:
        // case 0 reads version numbers
        final Matcher m = Pattern.compile("^\\d+").matcher(v);
        if (!m.lookingAt()) throw new VersionFormatException();
        try {
          n = Integer.parseInt(m.group());
          if (n < 0) throw new VersionFormatException();
        }
        catch (NumberFormatException e) {
          throw new VersionFormatException(e);
        }
        v = v.substring(m.end());
        state = v.length() == 0 ? 4 : 1;
        return n;
      case 1:
        // case 1 eats delimiters
        switch (v.charAt(0)) {
        case '.':
          state = 0;
          v = v.substring(1);
          break;
        case '-':
          state = 2;
          v = v.substring(1);
          return -2;
        default:
          throw new VersionFormatException();
        }
        break;
      case 2:
        // case 2 reads the tag type
        if (v.startsWith("svn")) {
          v = v.substring(3);
          state = 3;
          return 0;
        }
        else if (v.startsWith("beta")) {
          v = v.substring(4);
          state = 3;
          return 1;
        }
        else if (v.startsWith("rc")) {
          v = v.substring(2);
          state = 3;
          return 2;
        }
        throw new VersionFormatException();
      case 3:
        // case 3 reads the tag number
        try {
          n = Integer.parseInt(v);
          if (n < 0) throw new VersionFormatException();
        }
        catch (NumberFormatException e) {
          throw new VersionFormatException(e);
        }
        v = "";
        state = 4;
        return n;
      case 4:
        // case 4 returns the end-of-string marker
        state = 5;
        return -1;
      case 5:
        // case 5 is terminal
      default:
        throw new IllegalStateException();
      }
    }
  }

  /**
   * Tests for <code>VersionTokenizer</code>. A "<code>!</code>" is
   * printed whenever parsing fails.
   *
   * @param args strings to parse
   */
  public static void main(String[] args) {
    String[] ex;
    if (args.length > 0) {  // tokenize user input
      ex = args;
    }
    else {  // tokenize these examples
      ex = new String[] {
        "1.2.3",
        "1.2.3.4",
        "1.2.3-svn7890",
        "1.2.3-beta5",
        "1.2.3-rc3",
        "foobarbaz",
        "1.2.foo",
        "1.2-foo",
        "1.2-svn1234.8",
        "3.0b6"
      };
    }

    for (String v : ex) {
      System.out.print(v + ":");
      final VersionTokenizer tok = new VersionTokenizer(v);
      try {
        while (tok.hasNext()) System.out.print(" " + tok.next());
        System.out.print("\n");
      }
      catch (VersionFormatException e) {
        System.out.print(" !\n");
      }
    }
    System.out.print("\n");
  } 
}
