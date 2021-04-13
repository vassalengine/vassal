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

import VASSAL.Info;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.util.concurrent.CancellationException;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ProcessCallableTest {

  @Test
  public void testNormal() throws Exception {
    final String EOL = System.lineSeparator();

    final byte[] eout = ("Jackdaws love my big sphinx of quartz." + EOL).getBytes();
    final byte[] eerr = ("Veldt jynx grimps waqf zho buck." + EOL).getBytes();

    final ProcessBuilder pb = new ProcessBuilder(
      Info.getJavaBinPath().getAbsolutePath(),
      "-cp",
      System.getProperty("java.class.path"),
      "VASSAL.tools.io.ProcessCallableTestEchoer"
    );

    final Process proc = pb.start();

    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    final ByteArrayOutputStream err = new ByteArrayOutputStream();

    final ExecutorService exec = Executors.newCachedThreadPool();

    final InputOutputStreamPump outPump = new InputOutputStreamPump(null, out);
    final InputOutputStreamPump errPump = new InputOutputStreamPump(null, err);

    final ProcessCallable c = new ProcessCallable(proc, outPump, errPump, exec);
    final Future<Integer> f = exec.submit(c);

    final OutputStream pin = proc.getOutputStream();

    pin.write(eout);
    pin.write(eerr);
    pin.write(("42" + EOL).getBytes());
    pin.close();

    assertEquals(42, (int) f.get());
    assertArrayEquals(eout, out.toByteArray());
    assertArrayEquals(eerr, err.toByteArray());
  }

  @Test
  public void testInterrupt() throws Exception {
    final ProcessBuilder pb = new ProcessBuilder(
      "java",
      "-cp",
      System.getProperty("java.class.path"),
      "VASSAL.tools.io.ProcessCallableTestBlocker"
    );

    final Process proc = pb.start();

    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    final ByteArrayOutputStream err = new ByteArrayOutputStream();

    final ExecutorService exec = Executors.newCachedThreadPool();

    final InputOutputStreamPump outPump = new InputOutputStreamPump(null, out);
    final InputOutputStreamPump errPump = new InputOutputStreamPump(null, err);

    final ProcessCallable c = new ProcessCallable(proc, outPump, errPump, exec);
    final Future<Integer> f = exec.submit(c);

    f.cancel(true);

    try {
      f.get();
      fail();
    }
    catch (CancellationException e) {
      // expected
    }

    assertArrayEquals(new byte[0], out.toByteArray());
    assertArrayEquals(new byte[0], err.toByteArray());
  }
}
