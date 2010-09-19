/*
 * $Id$
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

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.*;

@RunWith(JMock.class)
public class LoggedOutputStreamTest {

  public final Mockery context = new JUnit4Mockery();

  @Test
  public void testWriteInt() {
    final Logger logger = context.mock(Logger.class);
    final LoggedOutputStream out = new LoggedOutputStream(logger);

    context.checking(new Expectations() {
      {
        oneOf(logger).warn("xyzzy");
        oneOf(logger).warn("foo");
      }
    });

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
  }

  @Test
  public void testWriteArray() {
    final Logger logger = context.mock(Logger.class);
    final LoggedOutputStream out = new LoggedOutputStream(logger);

    context.checking(new Expectations() {
      {
        oneOf(logger).warn("xyzzy");
        oneOf(logger).warn("foo");
      }
    });

    final byte[] xyzzy = new byte[] { 'x', 'y', 'z', 'z', 'y', '\n' };
    out.write(xyzzy, 0, xyzzy.length);

    final byte[] foo = new byte[] { 'f', 'o', 'o' };
    out.write(foo, 0, foo.length);
  }

  @Test
  public void testEmptyFlush() {
    final Logger logger = context.mock(Logger.class);
    final LoggedOutputStream out = new LoggedOutputStream(logger);

    context.checking(new Expectations() {
      {
        never(logger).warn(with(any(String.class)));
      }
    });

    out.write((byte) '\n');
  }
}
