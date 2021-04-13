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

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.*;

import java.util.stream.Stream;

@SuppressWarnings("removal")
@Deprecated(since = "2020-08-28", forRemoval = true)
public class VassalVersionTokenizerTest {

  @ParameterizedTest
  @MethodSource("testData")
  public void testTokenizer(String inputVersionString, Object[] expectedResult) throws VersionFormatException {
    final VassalVersionTokenizer tok = new VassalVersionTokenizer(inputVersionString);
    for (Object nextExpected : expectedResult) {
      if (nextExpected != null) {
        assertEquals(nextExpected, tok.next(), inputVersionString);
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

  /**
   * The first element is the input, the second is the array of expected tokens.
   * A null indicates that a VersionFormatException is expected.
   */
  private static Stream<Arguments> testData() {
    return Stream.of(
        Arguments.of("1.2.3",                   new Object[] { 1, 2, 3         }),
        Arguments.of("1.2.3.4",                 new Object[] { 1, 2, 3, 4      }),
        Arguments.of("1.2.3-rc3",               new Object[] { 1, 2, 3, null   }),
        Arguments.of("foobarbaz",               new Object[] { null            }),
        Arguments.of("1.2.foo",                 new Object[] { 1, 2, null      }),
        Arguments.of("3.0b6",                   new Object[] { 3, 0, null      }),
        Arguments.of("3.3.1-test",              new Object[] { 3, 3, 1, -1     }),
        Arguments.of("3.3.1-test-80",           new Object[] { 3, 3, 1, -1, 80 }),
        Arguments.of("3.3.1-test-80-gf8ef2523", new Object[] { 3, 3, 1, -1, 80 }),
        Arguments.of("3.3.1-80",                new Object[] { 3, 3, 1, 80     }),
        Arguments.of("3.3.1-80-gf8ef2523",      new Object[] { 3, 3, 1, 80     }));
  }

}
