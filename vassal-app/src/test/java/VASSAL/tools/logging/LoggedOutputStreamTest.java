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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.logging;

import org.slf4j.Logger;

import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.*;

public class LoggedOutputStreamTest {

  @Test
  public void testWriteInt() {
    final Logger logger = mock(Logger.class);
    final LoggedOutputStream out = new LoggedOutputStream(logger);

    out.write((byte) 'x');
    out.write((byte) 'y');
    out.write((byte) 'z');
    out.write((byte) 'z');
    out.write((byte) 'y');
    out.write((byte) '\n');
    out.write((byte) 'f');
    out.write((byte) 'o');
    out.write((byte) 'o');
    out.write((byte) '\n');

    verify(logger, atMostOnce()).warn(eq("xyzzy"));
    verify(logger, atMostOnce()).warn(eq("foo"));
  }

  @Test
  public void testWriteArray() {
    final Logger logger = mock(Logger.class);
    final LoggedOutputStream out = new LoggedOutputStream(logger);

    final byte[] xyzzy = new byte[] { 'x', 'y', 'z', 'z', 'y', '\n' };
    out.write(xyzzy, 0, xyzzy.length);

    final byte[] foo = new byte[] { 'f', 'o', 'o' };
    out.write(foo, 0, foo.length);

    verify(logger, atMostOnce()).warn(eq("xyzzy"));
    verify(logger, atMostOnce()).warn(eq("foo"));
  }

  @Test
  public void testEmptyFlush() {
    final Logger logger = mock(Logger.class);
    final LoggedOutputStream out = new LoggedOutputStream(logger);

    out.write((byte) '\n');

    verify(logger, never()).warn(anyString());
  }

}
