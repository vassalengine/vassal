/*
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
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;

@SuppressWarnings("removal")
@Deprecated(since = "2020-08-28", forRemoval = true)
@RunWith(Parameterized.class)
public class VassalVersionTokenizerTest {

  private final String inputVersionString;
  private final Object[] expectedResult;

  public VassalVersionTokenizerTest(String inputVersionString, Object[] expectedResult) {
    this.inputVersionString = inputVersionString;
    this.expectedResult = expectedResult;
  }

  /**
   * The first element is the input, the second is the array of expected tokens.
   * A null indicates that a VersionFormatException is expected.
   */
  @Parameterized.Parameters
  public static Collection<Object[]> testData() {
    return Arrays.asList(new Object[][]{
      { "1.2.3",                   new Object[] { 1, 2, 3         }},
      { "1.2.3.4",                 new Object[] { 1, 2, 3, 4      }},
      { "1.2.3-rc3",               new Object[] { 1, 2, 3, null   }},
      { "foobarbaz",               new Object[] { null            }},
      { "1.2.foo",                 new Object[] { 1, 2, null      }},
      { "3.0b6",                   new Object[] { 3, 0, null      }},
      { "3.3.1-test",              new Object[] { 3, 3, 1, -1     }},
      { "3.3.1-test-80",           new Object[] { 3, 3, 1, -1, 80 }},
      { "3.3.1-test-80-gf8ef2523", new Object[] { 3, 3, 1, -1, 80 }},
      { "3.3.1-80",                new Object[] { 3, 3, 1, 80     }},
      { "3.3.1-80-gf8ef2523",      new Object[] { 3, 3, 1, 80     }}
    });
  }

  @Test
  public void testTokenizer() throws VersionFormatException {
    final VassalVersionTokenizer tok = new VassalVersionTokenizer(inputVersionString);
    for (Object nextExpected : expectedResult) {
      if (nextExpected != null) {
        assertEquals(inputVersionString, nextExpected, tok.next());
      }
      else {
        try {
          tok.next();
        }
        catch (VersionFormatException e) {
          // This is expected.
          continue;
        }

        fail("expected exception from " + inputVersionString + ", token " + nextExpected);
      }
    }
  }
}
