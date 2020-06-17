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

import org.junit.Test;

import static org.junit.Assert.*;

public class SimpleVersionTokenizerTest {
  @Test
  public void testTokenizer() throws VersionFormatException {
    // The first element is the input, following elements are the
    // expected tokens. A null indicates that a VersionFormatException
    // is expected.
    final Object[][] versions = {
      { "1.2.3",         1, 2, 3       },
      { "1.2.3.4",       1, 2, 3, 4    },
      { "1.2.3-svn7890", 1, 2, 3, null },
      { "1.2.3-beta5",   1, 2, 3, null },
      { "1.2.3-rc3",     1, 2, 3, null },
      { "foobarbaz",     null          },
      { "1.2.foo",       1, 2, null    },
      { "1.2-foo",       1, 2, null    },
      { "1.2-svn1234.8", 1, 2, null    },
      { "3.0b6",         3, 0, null    }
    };

    for (Object[] v : versions) {
      final String vs = (String) v[0];
      final SimpleVersionTokenizer tok = new SimpleVersionTokenizer(vs);
      for (int i = 1; i < v.length; ++i) {
        if (v[i] != null) {
          assertEquals(vs, v[i], tok.next());
        }
        else {
          try {
            tok.next();
          }
          catch (VersionFormatException e) {
            // This is expected.
            continue;
          }

          fail("expected exception from " + vs + ", token " + i);
        }
      }
    }
  }
}
