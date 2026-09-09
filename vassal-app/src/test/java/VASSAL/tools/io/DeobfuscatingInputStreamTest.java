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
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class DeobfuscatingInputStreamTest {
  // A popular pangram.
  private final String plain = "All jackdaws love my great sphinx of quartz.";

  // The same pangram, obfuscated in the legacy hex-encoded format.
  private final String legacyObfus = "!VCSK581934347832393b333c392f2b7834372e3d783521783f2a3d392c782b283031362078373e78292d392a2c2276";

  private static byte[] deobfuscate(byte[] b) throws IOException {
    try (DeobfuscatingInputStream in =
           new DeobfuscatingInputStream(new ByteArrayInputStream(b))) {
      return in.readAllBytes();
    }
  }

  private static byte[] obfuscate(byte[] b) throws IOException {
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();
    try (ObfuscatingOutputStream out = new ObfuscatingOutputStream(bout)) {
      out.write(b);
    }
    return bout.toByteArray();
  }

  /** Test plain text input. */
  @Test
  public void testPlainInput() throws IOException {
    final byte[] expected = plain.getBytes(StandardCharsets.UTF_8);
    assertArrayEquals(expected, deobfuscate(expected));
  }

  /** Test plain text input shorter than a header. */
  @Test
  public void testShortPlainInput() throws IOException {
    final byte[] expected = "ab".getBytes(StandardCharsets.UTF_8);
    assertArrayEquals(expected, deobfuscate(expected));
  }

  /** Test empty input. */
  @Test
  public void testEmptyInput() throws IOException {
    assertArrayEquals(new byte[0], deobfuscate(new byte[0]));
  }

  /** Test plain text input which is exactly as long as the header. */
  @Test
  public void testHeaderLengthPlainInput() throws IOException {
    final byte[] expected = "abcde".getBytes(StandardCharsets.UTF_8);
    assertEquals(ObfuscatingOutputStream.HEADER_BYTES.length, expected.length);
    assertArrayEquals(expected, deobfuscate(expected));
  }

  /** Test obfuscated input. */
  @Test
  public void testObfuscatedInput() throws IOException {
    final byte[] expected = plain.getBytes(StandardCharsets.UTF_8);
    assertArrayEquals(expected, deobfuscate(obfuscate(expected)));
  }

  /** Test obfuscated input containing every byte value. */
  @Test
  public void testObfuscatedInputAllByteValues() throws IOException {
    final byte[] expected = new byte[256];
    for (int i = 0; i < expected.length; ++i) {
      expected[i] = (byte) i;
    }
    assertArrayEquals(expected, deobfuscate(obfuscate(expected)));
  }

  /** Test empty obfuscated input. */
  @Test
  public void testEmptyObfuscatedInput() throws IOException {
    assertArrayEquals(new byte[0], deobfuscate(obfuscate(new byte[0])));
  }

  /** Test obfuscated input read one byte at a time. */
  @Test
  public void testObfuscatedInputByByte() throws IOException {
    final byte[] expected = plain.getBytes(StandardCharsets.UTF_8);
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();

    try (DeobfuscatingInputStream in = new DeobfuscatingInputStream(
           new ByteArrayInputStream(obfuscate(expected)))) {
      int b;
      while ((b = in.read()) >= 0) {
        bout.write(b);
      }
    }

    assertArrayEquals(expected, bout.toByteArray());
  }

  /** An obfuscated stream lacking a key is malformed. */
  @Test
  public void testMissingKey() {
    final byte[] b = ObfuscatingOutputStream.HEADER_BYTES.clone();
    assertThrows(IOException.class, () -> deobfuscate(b));
  }

  /** Test legacy obfuscated input with lowercase hex digits. */
  @Test
  public void testLegacyObfuscatedInputLowerCaseHexDigits() throws IOException {
    final byte[] expected = plain.getBytes(StandardCharsets.UTF_8);
    final byte[] b = legacyObfus.getBytes(StandardCharsets.UTF_8);
    assertArrayEquals(expected, deobfuscate(b));
  }

  /** Test legacy obfuscated input with uppercase hex digits. */
  @Test
  public void testLegacyObfuscatedInputUpperCaseHexDigits() throws IOException {
    final byte[] expected = plain.getBytes(StandardCharsets.UTF_8);
    final byte[] b = legacyObfus.toUpperCase().getBytes(StandardCharsets.UTF_8);
    assertArrayEquals(expected, deobfuscate(b));
  }
}
