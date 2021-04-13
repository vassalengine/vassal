/*
 *
 * Copyright (c) 2010 by Joel Uckelman
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
import java.io.InputStream;
import java.io.IOException;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.input.NullInputStream;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

public class RereadableInputStreamTest {
  @Test
  public void testMarkSupported() {
    final InputStream in = new RereadableInputStream(new NullInputStream(10));
    assertTrue(in.markSupported());
  }

  @Test
  public void testReadInt() throws IOException {
    final byte[] expected = new byte[]{ 0, 1, 2, 3, 4, 5, 6, 7 };
    final InputStream in =
      new RereadableInputStream(new ByteArrayInputStream(expected));

    for (int i = 0; i < expected.length; ++i) {
      assertEquals(expected[i], in.read());
    }

    assertEquals(-1, in.read());
  }

  @Test
  public void testReadBytes() throws IOException {
    final byte[] expected = new byte[]{ 0, 1, 2, 3, 4, 5, 6, 7 };
    final InputStream in =
      new RereadableInputStream(new ByteArrayInputStream(expected));

    final byte[] actual = new byte[expected.length];
    final int count = in.read(actual);

    assertEquals(-1, in.read());
    assertArrayEquals(expected, actual);
  }

  @Test
  public void testResetBad() {
    final InputStream in = new RereadableInputStream(new NullInputStream(10));
    assertThrows(IOException.class, () -> in.reset());
  }

  @Test
  public void testMarkAndReset() throws IOException {
    final byte[] expected = new byte[]{ 0, 1, 2, 3, 4, 5, 6, 7 };
    final InputStream in =
      new RereadableInputStream(new ByteArrayInputStream(expected));

    in.mark(4);

    int count;

    final byte[] buf = new byte[4];
    count = in.read(buf, 0, 4);

    assertEquals(4, count);

    in.reset();

    final byte[] actual = new byte[expected.length];
    count = IOUtils.read(in, actual);

    assertEquals(expected.length, count);
    assertEquals(-1, in.read());
    assertArrayEquals(expected, actual);
  }

  @Test
  public void testClose() throws IOException {
    final InputStream child = mock(InputStream.class);

    final InputStream in = new RereadableInputStream(child);
    in.close();

    verify(child, times(1)).close();
  }
}
