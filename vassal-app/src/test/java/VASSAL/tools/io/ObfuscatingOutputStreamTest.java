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

import org.junit.*;
import static org.junit.jupiter.api.Assertions.*;

public class ObfuscatingOutputStreamTest {
  // A popular pangram.
  private final String plain = "All jackdaws love my great sphinx of quartz.";

  // The same pangram, obfuscated.
  private final String obfus = "!VCSK581934347832393b333c392f2b7834372e3d783521783f2a3d392c782b283031362078373e78292d392a2c2276";

  // The key used for the obfuscated text.
  private final byte key = (byte) 0x58;

  @Test
  public void testObfuscatedOutput() throws IOException {
    final byte[] expected = obfus.getBytes("UTF-8");
    final ByteArrayOutputStream bout = new ByteArrayOutputStream();

    final ObfuscatingOutputStream out = new ObfuscatingOutputStream(bout, key);
    out.write(plain.getBytes("UTF-8"));
    out.close();

    assertArrayEquals(expected, bout.toByteArray());
  }
}
