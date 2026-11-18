/*
 * Copyright (c) 2026 by the VASSAL Development Team
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

package VASSAL.tools.io;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Whether an argument needs encoding is a question about the machine's ANSI
 * code page, so these drive the charset directly instead of asking the host
 * JVM what its own code page happens to be.
 */
public class ArgEncodingTest {

  /** The module path from vassalengine/vassal#14193, ligature and all. */
  private static final String REPORTED_PATH =
    "D:\\Games\\Wargames\\Tabletop Vassal\\La Bataille d'Auerst\u00e6dt"
      + "\\La_Bataille_d'Auerstedt_1.18.vmod";

  private static final String ASCII_PATH =
    "D:\\Games\\Wargames\\Tabletop Vassal\\Auerstedt\\Auerstedt_1.18.vmod";

  private static final Charset CP1250 = Charset.forName("windows-1250");
  private static final Charset CP1252 = Charset.forName("windows-1252");

  /**
   * The report itself: the path has no character above U+00FF, so the test
   * which only looked above U+00FF passed it through raw, and the tiler was
   * handed "Auerst?dt". Any code page without the ligature does this, and
   * most Windows code pages are without it.
   */
  @Test
  public void reportedPathNeedsEncodingWhereCodePageLacksIt() {
    assertTrue(ArgEncoding.requires(REPORTED_PATH, CP1250));
  }

  /** On a windows-1252 machine the same path crosses intact, as it always did. */
  @Test
  public void reportedPathIsFineOnWindows1252() {
    assertFalse(ArgEncoding.requires(REPORTED_PATH, CP1252));
  }

  /** As it does where the code page has been set to UTF-8. */
  @Test
  public void reportedPathIsFineOnUtf8() {
    assertFalse(ArgEncoding.requires(REPORTED_PATH, StandardCharsets.UTF_8));
  }

  /** An ASCII path is safe everywhere, including the fallback charset. */
  @Test
  public void asciiPathNeverNeedsEncoding() {
    assertFalse(ArgEncoding.requires(ASCII_PATH, CP1250));
    assertFalse(ArgEncoding.requires(ASCII_PATH, CP1252));
    assertFalse(ArgEncoding.requires(ASCII_PATH, StandardCharsets.UTF_8));
    assertFalse(ArgEncoding.requires(ASCII_PATH, StandardCharsets.US_ASCII));
  }

  /**
   * The other half of the old test being the wrong question: U+20AC is above
   * U+00FF and so was always encoded, although windows-1252 holds it and it
   * would have crossed untouched.
   */
  @Test
  public void characterAboveFFWhichTheCodePageHoldsNeedsNoEncoding() {
    assertFalse(ArgEncoding.requires("C:\\Pre\u20accio\\mod.vmod", CP1252));
  }

  /** And one it does not hold still does. */
  @Test
  public void characterTheCodePageLacksNeedsEncoding() {
    assertTrue(ArgEncoding.requires("C:\\\u65e5\u672c\\mod.vmod", CP1252));
  }

  /** Nothing to encode, nothing to ask. */
  @Test
  public void nullNeedsNoEncoding() {
    assertFalse(ArgEncoding.requires(null, StandardCharsets.US_ASCII));
    assertFalse(ArgEncoding.requires(null));
  }

  /**
   * What is encoded must come back, and must be pure ASCII in between --
   * that is the entire point of encoding it.
   */
  @Test
  public void encodingRoundTrips() {
    final String encoded = ArgEncoding.encode(REPORTED_PATH);
    assertTrue(StandardCharsets.US_ASCII.newEncoder().canEncode(encoded));
    assertEquals(REPORTED_PATH, ArgEncoding.decode(encoded));
    assertEquals(REPORTED_PATH, ArgEncoding.decode(true, encoded).getPath());
    assertEquals(ASCII_PATH, ArgEncoding.decode(false, ASCII_PATH).getPath());
  }
}
