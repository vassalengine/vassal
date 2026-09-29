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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.tukaani.xz.LZMA2Options;
import org.tukaani.xz.XZOutputStream;
import static org.junit.jupiter.api.Assertions.*;

public class ObfuscatingOutputStreamTest {
  // A popular pangram.
  private final String plain = "All jackdaws love my great sphinx of quartz.";

  // The key used for the obfuscated text.
  private final byte key = (byte) 0x58;

  // A key with the high bit set, to catch sign-extension errors.
  private final byte highKey = (byte) 0x80;

  /** The expected output: the XZ header, the key, then the XZ-compressed text XORed with the key. */
  private byte[] obfuscated(byte key) throws IOException {
    return obfuscated(plain.getBytes(StandardCharsets.UTF_8), key);
  }

  private static byte[] obfuscated(byte[] plainBytes, byte key) throws IOException {
    final ByteArrayOutputStream xz = new ByteArrayOutputStream();
    try (XZOutputStream out = new XZOutputStream(xz, new LZMA2Options(ObfuscatingOutputStream.PRESET))) {
      out.write(plainBytes);
    }
    final byte[] compressed = xz.toByteArray();
    final byte[] header = ObfuscatingOutputStream.XZ_HEADER_BYTES;

    final byte[] expected = new byte[header.length + 1 + compressed.length];
    System.arraycopy(header, 0, expected, 0, header.length);
    expected[header.length] = key;
    for (int i = 0; i < compressed.length; ++i) {
      expected[header.length + 1 + i] = (byte) (compressed[i] ^ key);
    }

    return expected;
  }

  private byte[] writeAsArray(byte key) throws IOException {
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();

    try (ObfuscatingOutputStream out =
           new ObfuscatingOutputStream(bout, key)) {
      out.write(plain.getBytes(StandardCharsets.UTF_8));
    }

    return bout.toByteArray();
  }

  private byte[] writeByByte(byte key) throws IOException {
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();

    try (ObfuscatingOutputStream out =
           new ObfuscatingOutputStream(bout, key)) {
      for (final byte b : plain.getBytes(StandardCharsets.UTF_8)) {
        out.write(b);
      }
    }

    return bout.toByteArray();
  }

  /** Test writing an array. */
  @Test
  public void testObfuscatedOutput() throws IOException {
    assertArrayEquals(obfuscated(key), writeAsArray(key));
  }

  /** Test writing one byte at a time. */
  @Test
  public void testObfuscatedOutputByByte() throws IOException {
    assertArrayEquals(obfuscated(key), writeByByte(key));
  }

  /** Test writing an array with a key which has the high bit set. */
  @Test
  public void testObfuscatedOutputHighKey() throws IOException {
    assertArrayEquals(obfuscated(highKey), writeAsArray(highKey));
  }

  /** Test writing one byte at a time with a key with the high bit set. */
  @Test
  public void testObfuscatedOutputByByteHighKey() throws IOException {
    assertArrayEquals(obfuscated(highKey), writeByByte(highKey));
  }

  /** The caller's array must not be modified. */
  @Test
  public void testInputArrayUnmodified() throws IOException {
    final byte[] bytes = plain.getBytes(StandardCharsets.UTF_8);
    final byte[] copy = bytes.clone();

    try (ObfuscatingOutputStream out =
           new ObfuscatingOutputStream(new ByteArrayOutputStream(), key)) {
      out.write(bytes);
    }

    assertArrayEquals(copy, bytes);
  }

  /** A large write must be obfuscated correctly throughout. */
  @Test
  public void testObfuscatedOutputLargeArray() throws IOException {
    final byte[] bytes = new byte[100000];
    for (int i = 0; i < bytes.length; ++i) {
      bytes[i] = (byte) i;
    }
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();
    try (ObfuscatingOutputStream out =
           new ObfuscatingOutputStream(bout, key)) {
      out.write(bytes);
    }
    assertArrayEquals(obfuscated(bytes, key), bout.toByteArray());
  }

  /** Repetitive text, as a saved game is, must come out far smaller than it went in. */
  @Test
  public void testRepetitiveInputIsCompressed() throws IOException {
    final StringBuilder sb = new StringBuilder();
    for (int i = 0; i < 2000; ++i) {
      sb.append(plain).append(' ').append(i % 7).append('\n');
    }
    final byte[] bytes = sb.toString().getBytes(StandardCharsets.UTF_8);
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();
    try (ObfuscatingOutputStream out =
           new ObfuscatingOutputStream(bout, key)) {
      out.write(bytes);
    }
    assertTrue(bout.size() < bytes.length / 20, "compressed " + bytes.length + " to " + bout.size());
  }

  /** The key is never zero, as XORing with zero obfuscates nothing. */
  @Test
  public void testKeyIsNeverZero() throws IOException {
    final int off = ObfuscatingOutputStream.XZ_HEADER_BYTES.length;

    for (int i = 0; i < 10000; ++i) {
      final ByteArrayOutputStream bout = new ByteArrayOutputStream();
      new ObfuscatingOutputStream(bout).close();
      assertTrue(bout.toByteArray()[off] != 0);
    }
  }
}
