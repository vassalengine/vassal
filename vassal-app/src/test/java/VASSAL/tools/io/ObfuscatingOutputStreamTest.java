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

import org.junit.*;
import static org.junit.jupiter.api.Assertions.*;

public class ObfuscatingOutputStreamTest {
  // A popular pangram.
  private final String plain = "All jackdaws love my great sphinx of quartz.";

  // The key used for the obfuscated text.
  private final byte key = (byte) 0x58;

  @Test
  public void testObfuscatedOutputFormat() throws IOException {
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();

    final ObfuscatingOutputStream out = new ObfuscatingOutputStream(bout, key);
    out.write(plain.getBytes("UTF-8"));
    out.close();

    final String result = new String(bout.toByteArray(), "UTF-8");

    // header, then the key in hex
    assertTrue(result.startsWith(ObfuscatingOutputStream.DEFLATED_HEADER + "58"));

    // everything after the header is lowercase hex
    final String body = result.substring(ObfuscatingOutputStream.DEFLATED_HEADER.length());
    assertTrue(body.matches("[0-9a-f]+"));
  }

  @Test
  public void testRoundTrip() throws IOException {
    final byte[] expected = plain.getBytes("UTF-8");
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();

    final ObfuscatingOutputStream out = new ObfuscatingOutputStream(bout, key);
    out.write(expected);
    out.close();

    final DeobfuscatingInputStream in = new DeobfuscatingInputStream(
      new ByteArrayInputStream(bout.toByteArray()));
    final byte[] result = in.readAllBytes();
    in.close();

    assertArrayEquals(expected, result);
  }
}
