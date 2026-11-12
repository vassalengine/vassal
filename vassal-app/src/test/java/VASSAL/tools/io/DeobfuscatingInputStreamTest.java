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
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import org.tukaani.xz.XZInputStream;

import VASSAL.build.module.GameState;

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

  /** Compresses as a saved game is written. */
  private static byte[] obfuscate(byte[] b) throws IOException {
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();
    try (OutputStream out = GameState.compressSavedGame(bout)) {
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

  /** Test plain text input which is exactly as long as the XZ magic. */
  @Test
  public void testHeaderLengthPlainInput() throws IOException {
    final byte[] expected = "abcdef".getBytes(StandardCharsets.UTF_8);
    assertArrayEquals(expected, deobfuscate(expected));
  }

  /** Test plain text input which is exactly as long as the legacy header. */
  @Test
  public void testLegacyHeaderLengthPlainInput() throws IOException {
    final byte[] expected = "abcde".getBytes(StandardCharsets.UTF_8);
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

  /** What is written is an XZ stream, and nothing else: it begins with the XZ magic. */
  @Test
  public void testOutputIsXz() throws IOException {
    final byte[] b = obfuscate(plain.getBytes(StandardCharsets.UTF_8));
    assertArrayEquals(new byte[] {(byte) 0xFD, '7', 'z', 'X', 'Z', 0}, Arrays.copyOf(b, 6));
    try (XZInputStream in = new XZInputStream(new ByteArrayInputStream(b))) {
      assertArrayEquals(plain.getBytes(StandardCharsets.UTF_8), in.readAllBytes());
    }
  }

  /** Repetitive text, as a saved game is, must come out far smaller than it went in. */
  @Test
  public void testRepetitiveInputIsCompressed() throws IOException {
    final StringBuilder sb = new StringBuilder();
    for (int i = 0; i < 2000; ++i) {
      sb.append(plain).append(' ').append(i % 7).append('\n');
    }
    final byte[] bytes = sb.toString().getBytes(StandardCharsets.UTF_8);
    final byte[] b = obfuscate(bytes);
    assertTrue(b.length < bytes.length / 20, "compressed " + bytes.length + " to " + b.length);
    assertArrayEquals(bytes, deobfuscate(b));
  }

  /** A truncated XZ stream is malformed. */
  @Test
  public void testTruncatedXz() throws IOException {
    final byte[] b = obfuscate(plain.getBytes(StandardCharsets.UTF_8));
    assertThrows(IOException.class, () -> deobfuscate(Arrays.copyOf(b, b.length / 2)));
  }

  /** The deprecated ObfuscatingOutputStream now writes the same compressed form. */
  @SuppressWarnings("removal")
  @Test
  public void testDeprecatedObfuscatingOutputStreamWritesXz() throws IOException {
    final byte[] expected = plain.getBytes(StandardCharsets.UTF_8);
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();
    try (ObfuscatingOutputStream out = new ObfuscatingOutputStream(bout, (byte) 0x58)) {
      out.write(expected);
    }
    assertArrayEquals(obfuscate(expected), bout.toByteArray());
    assertArrayEquals(expected, deobfuscate(bout.toByteArray()));
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
