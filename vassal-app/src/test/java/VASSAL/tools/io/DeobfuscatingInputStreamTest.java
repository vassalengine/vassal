/*
 *
 * Copyright (c) 2009 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools.io;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.junit.*;
import static org.junit.jupiter.api.Assertions.*;

public class DeobfuscatingInputStreamTest {
  // A popular pangram.
  private final String plain = "All jackdaws love my great sphinx of quartz.";

  // The same pangram, obfuscated.
  private final String obfus = "!VCSK581934347832393b333c392f2b7834372e3d783521783f2a3d392c782b283031362078373e78292d392a2c2276";

  /** Test plain text input. */
  @Test
  public void testPlainInput() throws IOException {
    final byte[] expected = plain.getBytes("UTF-8");

    final DeobfuscatingInputStream in =
      new DeobfuscatingInputStream(
        new ByteArrayInputStream(expected));

    final byte[] result = in.readAllBytes();
    in.close();

    assertArrayEquals(expected, result);
  }

  /** Test obfuscated input with lowercase hex digits. */
  @Test
  public void testObfuscatedInputLowerCaseHexDigits() throws IOException {
    final byte[] b = obfus.getBytes("UTF-8");
    final byte[] expected = plain.getBytes("UTF-8");

    final DeobfuscatingInputStream in =
      new DeobfuscatingInputStream(
        new ByteArrayInputStream(b));

    final byte[] result = in.readAllBytes();
    in.close();

    assertArrayEquals(expected, result);
  }

  /** Test obfuscated input with uppercase hex digits. */
  @Test
  public void testObfuscatedInputUpperCaseHexDigits() throws IOException {
    final byte[] b = obfus.toUpperCase().getBytes("UTF-8");
    final byte[] expected = plain.getBytes("UTF-8");

    final DeobfuscatingInputStream in =
      new DeobfuscatingInputStream(
        new ByteArrayInputStream(b));

    final byte[] result = in.readAllBytes();
    in.close();

    assertArrayEquals(expected, result);
  }
}
