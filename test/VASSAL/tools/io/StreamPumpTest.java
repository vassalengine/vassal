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
import java.io.OutputStream;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import org.junit.Test;

import static org.junit.Assert.*;

public class StreamPumpTest {

  @Test
  public void testPumpNormal() throws Exception {
    final byte[] eout = "Jackdaws love my big sphinx of quartz.\n".getBytes();

    final ByteArrayInputStream in = new ByteArrayInputStream(eout);
    final ByteArrayOutputStream out = new ByteArrayOutputStream();

    final StreamPump pump = new StreamPump(in, out);

    final ExecutorService exec = Executors.newSingleThreadExecutor();
    final Future<?> f = exec.submit(pump);

    f.get();

    assertArrayEquals(eout, out.toByteArray());
  }

  @Test
  public void testPumpOutClosed() throws Exception {
    final byte[] eout = "Jackdaws love my big sphinx of quartz.\n".getBytes();

    final ByteArrayInputStream in = new ByteArrayInputStream(eout);
    final OutputStream out = new ClosedOutputStream();

    final StreamPump pump = new StreamPump(in, out);

    final ExecutorService exec = Executors.newSingleThreadExecutor();
    final Future<?> f = exec.submit(pump);

    f.get();
  }

  @Test
  public void testPumpInClosed() throws Exception {
    final InputStream in = new ClosedInputStream();
    final ByteArrayOutputStream out = new ByteArrayOutputStream();

    final StreamPump pump = new StreamPump(in, out);

    final ExecutorService exec = Executors.newSingleThreadExecutor();
    final Future<?> f = exec.submit(pump);

    f.get();

    assertArrayEquals(new byte[0], out.toByteArray());
  }

  @Test
  public void testPumpBothClosed() throws Exception {
    final InputStream in = new ClosedInputStream();
    final OutputStream out = new ClosedOutputStream();

    final StreamPump pump = new StreamPump(in, out);

    final ExecutorService exec = Executors.newSingleThreadExecutor();
    final Future<?> f = exec.submit(pump);

    f.get();
  }
}
