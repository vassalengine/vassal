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

import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A finite-state machine for converting VASSAL version numbers into
 * a series of integers. The integers thus returned from two different
 * tokenizers may be compared to determine the temporal ordering of two
 * VASSAL versions. Valid version strings are matched by the following
 * regular expression: <code>\d+(.\d+)*(-(svn\d+|.+)?</code>. Old
 * version numbers which are not valid by current standards (e.g., 3.0b6)
 * may be successfully parsed far enough to determine their ordering with
 * respect to post-3.1.0 versions.
 *
 * <p>Nonnumeric parts of a version string are tokenized as follows:</p>
 * <table>
 *  <tr><td>end-of-string</td><td>-1</td></tr>
 *  <tr><td><code>-</code> (tag delimiter)</td><td>-2</td></tr>
 *  <tr><td><code>svn\d+</code></td><td><code>\d+</code></td></tr>
 *  <tr><td>other tags</td><td>mapped to svn version</td></tr>
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
 * @see Version
 * @see VersionFormatException
 */
public class VassalVersionTokenizer implements VersionTokenizer {
  private String v;

  private enum State { NUM, DELIM, TAG, EOS, END };
  private State state = State.NUM;

  private static Map<String,Integer> tags = new HashMap<String,Integer>();

  // This is the mapping for tags to svn versions. Only tags which cannot
  // be distinguished from the current version from the numeric portion
  // alone need to be maintined here. (E.g., the 3.1.0 tags may be removed
  // as soon as 3.1.1 is released.)
  static {
    // 3.1.0
    tags.put("beta1", 3606);
    tags.put("beta2", 3664);
    tags.put("beta3", 4023);
  }

  /**
   * Constructs a <code>VersionTokenizer</code> which operates on a
   * version <code>String</code>.
   *
   * @param version the version <code>String</code> to parse
   * @throws IllegalArgumentException if <code>version == null</code>.
   */  
  public VassalVersionTokenizer(String version) {
    if (version == null) throw new IllegalArgumentException();
    v = version;
  }

  /** {@inheritDocs} */
  public boolean hasNext() {
    return v.length() > 0 || state == State.EOS;
  }

  /** {@inheritDocs} */
  public int next() throws VersionFormatException {
    if (!hasNext()) throw new NoSuchElementException(); 

    int n;

    while (true) {
      switch (state) {
      case NUM:   // read a version number
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
        state = v.length() == 0 ? State.EOS : State.DELIM;
        return n;
      case DELIM: // eat delimiters
        switch (v.charAt(0)) {
        case '.':
          state = State.NUM;
          v = v.substring(1);
          break;
        case '-':
          state = State.TAG;
          v = v.substring(1);
          return -2;
        default:
          throw new VersionFormatException();
        }
        break;
      case TAG: // parse the tag 
        if (v.startsWith("svn")) {
          // report the svn version
          v = v.substring(3);
          try {
            n = Integer.parseInt(v);
            if (n < 0) throw new VersionFormatException();
          }
          catch (NumberFormatException e) {
            throw new VersionFormatException(e);
          }
        }
        else if (tags.containsKey(v)) {
          // convert the tag to an svn version
          n = tags.get(v);
        }
        else throw new VersionFormatException();

        v = "";
        state = State.EOS;
        return n;
      case EOS: // mark the end of the string
        state = State.END;
        return -1;
      case END: // this case is terminal
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
        "3.1.0-beta2",
        "3.0b6"
      };
    }

    for (String v : ex) {
      System.out.print(v + ":");
      final VassalVersionTokenizer tok = new VassalVersionTokenizer(v);
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
