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
import java.util.Arrays;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.lib.legacy.ClassImposteriser;

import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.*;

public class CompositeInputStreamTest {
  protected final Mockery context = new Mockery() {
    {
      setImposteriser(ClassImposteriser.INSTANCE);
    }
  };

  protected InputStream[] prepareStreams() {
    final ByteArrayInputStream[] ch = new ByteArrayInputStream[10];
    for (int i = 0; i < ch.length; ++i) {
      final byte[] a = new byte[10];
      Arrays.fill(a, (byte) i);
      ch[i] = new ByteArrayInputStream(a);
    }
    return ch;
  }

  @Test
  public void testAvailable() throws IOException {
    final InputStream[] ch = prepareStreams();

    int available = 0;
    for (InputStream child : ch) available += child.available();

    final InputStream in = new CompositeInputStream(ch);
    assertEquals(available, in.available());
  }

  @Test
  public void testReadInt() throws IOException {
    final InputStream[] ch = prepareStreams();
    final InputStream in = new CompositeInputStream(ch);

    for (int i = 0; i < 100; ++i) {
      assertEquals(i/10, in.read());
    }

    assertEquals(-1, in.read());
  }

  @Test
  public void testReadBytes() throws IOException {
    final byte[] expected = new byte[100];
    for (int i = 0; i < 10; ++i) {
      Arrays.fill(expected, 10*i, 10*(i+1), (byte) i);
    }

    final InputStream[] ch = prepareStreams();
    final InputStream in = new CompositeInputStream(ch);

    final byte[] actual = new byte[100];
    final int count = IOUtils.read(in, actual);

    assertEquals(actual.length, count);
    assertEquals(-1, in.read());
    assertArrayEquals(expected, actual);
  }

  @Test
  public void testClose() throws IOException {
    final InputStream[] ch = new InputStream[10];
    final InputStream child = context.mock(InputStream.class);
    Arrays.fill(ch, child);

    context.checking(new Expectations() {
      {
        exactly(ch.length).of(child).close();
      }
    });

    final InputStream in = new CompositeInputStream(ch);
    in.close();
  }
}
