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
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;

import org.apache.commons.io.input.ClosedInputStream;
import org.apache.commons.io.output.ClosedOutputStream;

import VASSAL.tools.concurrent.listener.EventListener;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.*;

@RunWith(JMock.class)
public class InputOutputStreamPumpTest {
  protected final Mockery context = new JUnit4Mockery();

  protected static class IOSP extends InputOutputStreamPump {
    @Override
    public void run() {
      // Use a dummy run() for testing stream setting
      running = true;
    }
  }

  @Test
  public void testSetInputStreamNotRunning() {
    final InputOutputStreamPump p = new InputOutputStreamPump();
    final InputStream in = new ClosedInputStream();
    p.setInputStream(in);
  }

  @Test(expected=UnsupportedOperationException.class)
  public void testSetInputStreamRunning() {
    final InputOutputStreamPump p = new IOSP();
    p.run();

    final InputStream in = new ClosedInputStream();
    p.setInputStream(in);
  }

  @Test
  public void testSetOutputStreamNotRunning() {
    final InputOutputStreamPump p = new InputOutputStreamPump();
    final OutputStream out = new ClosedOutputStream();
    p.setOutputStream(out);
  }

  @Test(expected=UnsupportedOperationException.class)
  public void testSetOutputStreamRunning() {
    final InputOutputStreamPump p = new IOSP();
    p.run();

    final OutputStream out = new ClosedOutputStream();
    p.setOutputStream(out);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testPumpNormal() {
    final byte[] eout = "Jackdaws love my big sphinx of quartz.\n".getBytes();

    final ByteArrayInputStream in = new ByteArrayInputStream(eout);
    final ByteArrayOutputStream out = new ByteArrayOutputStream();

    final EventListener<IOException> el = context.mock(EventListener.class);
    context.checking(new Expectations() {
      {
        never(el).receive(with(any(Object.class)),
                          with(any(IOException.class)));
      }
    });

    final InputOutputStreamPump p = new InputOutputStreamPump(in, out, el);
    p.run();

    assertArrayEquals(eout, out.toByteArray());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testPumpOutClosed() {
    final byte[] eout = "Jackdaws love my big sphinx of quartz.\n".getBytes();

    final ByteArrayInputStream in = new ByteArrayInputStream(eout);
    final OutputStream out = new ClosedOutputStream();

    final EventListener<IOException> el = context.mock(EventListener.class);
    context.checking(new Expectations() {
      {
        oneOf(el).receive(with(aNonNull(InputOutputStreamPump.class)),
                          with(any(IOException.class)));
      }
    });

    final InputOutputStreamPump p = new InputOutputStreamPump(in, out, el);
    p.run();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testPumpInClosed() {
    final InputStream in = new ClosedInputStream();
    final ByteArrayOutputStream out = new ByteArrayOutputStream();

    final EventListener<IOException> el = context.mock(EventListener.class);
    context.checking(new Expectations() {
      {
        never(el).receive(with(any(Object.class)),
                          with(any(IOException.class)));
      }
    });

    final InputOutputStreamPump p = new InputOutputStreamPump(in, out, el);
    p.run();

    assertArrayEquals(new byte[0], out.toByteArray());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testPumpBothClosed() {
    final InputStream in = new ClosedInputStream();
    final OutputStream out = new ClosedOutputStream();

    final EventListener<IOException> el = context.mock(EventListener.class);
    context.checking(new Expectations() {
      {
        never(el).receive(with(any(Object.class)),
                          with(any(IOException.class)));
      }
    });

    final InputOutputStreamPump p = new InputOutputStreamPump(in, out, el);
    p.run();
  }
}
