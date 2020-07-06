/*
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

import java.util.Map;
import java.util.NoSuchElementException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A finite-state machine for converting VASSAL version numbers into
 * a series of integers. The integers thus returned from two different
 * tokenizers may be compared to determine the temporal ordering of two
 * VASSAL versions.
 *
 * Valid version strings are dotted decimal digits, followed optionally
 * by a tag, followed optionally by a build number. Anything beyond the
 * build nubmer is ignored.
 *
 * Old version numbers which are not valid by current standards (e.g., 3.0b6)
 * may be successfully parsed far enough to determine their ordering with
 * respect to post-3.1.0 versions.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @see Version
 * @see VersionFormatException
 */
public class VassalVersionTokenizer implements VersionTokenizer {
  protected String v;

  protected enum State {
    VNUM, DELIM1, TAG, DELIM2, BUILD, EOS, END
  }

  protected State state = State.VNUM;

  // This is the mapping for tags to versions. Only tags which cannot
  // be distinguished from the current version from the numeric portion
  // alone need to be maintined here. (E.g., the 3.1.0 tags can be removed
  // as soon as 3.1.1 is released.) We keep one tag for testing purposes.
  protected static Map<String,Integer> tags = Map.of(
    "test", -1
  );

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

  /** {@inheritDoc} */
  @Override
  public boolean hasNext() {
    return v.length() > 0 || state == State.EOS;
  }

  /** {@inheritDoc} */
  @Override
  public int next() throws VersionFormatException {
    if (!hasNext()) throw new NoSuchElementException();

    int n;

    while (true) {
      switch (state) {
      case VNUM:   // read a version number
        final Matcher m = Pattern.compile("^\\d+").matcher(v);
        if (!m.lookingAt()) throw new VersionFormatException();
        try {
          n = Integer.parseInt(m.group());
          if (n < 0) {
            throw new VersionFormatException();
          }
        }
        catch (NumberFormatException e) {
          throw new VersionFormatException(e);
        }
        v = v.substring(m.end());

        state = v.length() == 0 ? State.EOS : State.DELIM1;
        return n;

      case DELIM1: // eat delimiters
        switch (v.charAt(0)) {
        case '.':
          state = State.VNUM;
          v = v.substring(1);
          break;
        case '-':
          state = State.TAG;
          v = v.substring(1);
          break;
        default:
          throw new VersionFormatException();
        }
        break;

      case TAG: // parse the tag
        if (Character.isDigit(v.charAt(0))) {
          // this is a build number
          state = State.BUILD;
          break;
        }
        else {
          final int hi = v.indexOf('-');
          String k;
          if (hi == -1) {
            k = v;
            v = "";
            state = State.EOS;
          }
          else {
            k = v.substring(0, hi);
            v = v.substring(hi);
            state = State.DELIM2;
          }

          // convert the tag to a version
          final Integer i = tags.get(k);
          if (i == null) {
            throw new VersionFormatException();
          }

          return i;
        }

      case DELIM2:
        v = v.substring(1);
        state = State.BUILD;
        break;

      case BUILD: {
        // there may be stuff beyond the build number, but we don't care
        final int hi = v.indexOf('-');
        final String b = v.substring(0, hi == -1 ? v.length() : hi);

        try {
          n = Integer.parseInt(b);
        }
        catch (NumberFormatException e) {
          throw new VersionFormatException(e);
        }

        if (n < 0) {
          throw new VersionFormatException();
        }

        v = "";
        state = State.EOS;
        return n;
      }
      case EOS: // mark the end of the string
        state = State.END;
        return -1;

      case END: // this case is terminal
        throw new IllegalStateException();
      }
    }
  }
}
