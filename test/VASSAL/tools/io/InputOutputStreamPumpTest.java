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
import java.util.Arrays;

import org.apache.commons.io.input.ClosedInputStream;
import org.apache.commons.io.output.ClosedOutputStream;

import org.junit.Test;

import static org.junit.Assert.*;

public class InputOutputStreamPumpTest {

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
  public void testRun() {
    final byte[] expected = new byte[100];
    Arrays.fill(expected, (byte) 42);

    final ByteArrayInputStream in = new ByteArrayInputStream(expected);
    final ByteArrayOutputStream out = new ByteArrayOutputStream(); 

    final InputOutputStreamPump p = new InputOutputStreamPump(in, out);
    p.run();

    assertArrayEquals(expected, out.toByteArray()); 
  }  
}
