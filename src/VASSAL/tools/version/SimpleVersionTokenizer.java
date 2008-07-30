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

package VASSAL.tools.version;

import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A finite-state machine for converting version numbers into a series of
 * integers. The integers thus returned from two different tokenizers may
 * be compared to determine the temporal ordering of two versions. Valid
 * version strings are matched by the following regular expression:
 * <code>\d+(.\d+)*</code>. Invalid version numbers may be parsed up to
 * the point where they become invalid. The final token in any completely
 * parsed version is <code>-1</code>.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @see Version
 * @see VersionFormatException
 */
public class SimpleVersionTokenizer implements VersionTokenizer {
  protected String v;

  protected enum State { NUM, DELIM, EOS, END };
  protected State state = State.NUM;

  protected static Map<String,Integer> tags = new HashMap<String,Integer>();

  /**
   * Constructs a <code>VersionTokenizer</code> which operates on a
   * version <code>String</code>.
   *
   * @param version the version <code>String</code> to parse
   * @throws IllegalArgumentException if <code>version == null</code>.
   */  
  public SimpleVersionTokenizer(String version) {
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
        default:
          throw new VersionFormatException();
        }
        break;
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
        "3.0b6"
      };
    }

    for (String v : ex) {
      System.out.print(v + ":");
      final SimpleVersionTokenizer tok = new SimpleVersionTokenizer(v);
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
