/*
 *
 * Copyright (c) 2007-2010 by Joel Uckelman
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
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Random;

import org.apache.commons.io.FileUtils;

import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.*;

@RunWith(JMock.class)
public class IOUtilsTest {
  private final Mockery context = new JUnit4Mockery();

  @Test
  public void testCopyFileChannels() throws IOException {
    final File ifile = new File("src/test/resources/IOUtilsTest.txt");
    final File ofile = new File("target/test-classes/IOUtilsTest.out");

    try {
      final FileInputStream in = new FileInputStream(ifile);
      final FileOutputStream out = new FileOutputStream(ofile);

      final int count = IOUtils.copy(in, out);

      assertEquals(ifile.length(), count);
      assertTrue(FileUtils.contentEquals(ifile, ofile));
    }
    finally {
      ofile.delete();
    }
  }

  @Test
  public void testCopyLargeFileChannels() throws IOException {
    final File ifile = new File("src/test/resources/IOUtilsTest.txt");
    final File ofile = new File("target/test-classes/IOUtilsTest.out");

    try {
      final FileInputStream in = new FileInputStream(ifile);
      final FileOutputStream out = new FileOutputStream(ofile);

      final long count = IOUtils.copy(in, out);

      assertEquals(ifile.length(), count);
      assertTrue(FileUtils.contentEquals(ifile, ofile));
    }
    finally {
      ofile.delete();
    }
  }

  @Test
  public void testCopyBuffer() throws IOException {
    final byte[] buf = new byte[1024];

    final byte[] expected = new byte[10000];
    final long seed = System.currentTimeMillis();
    final Random rng = new Random(seed);
    rng.nextBytes(expected);

    final ByteArrayInputStream in = new ByteArrayInputStream(expected);
    final ByteArrayOutputStream out = new ByteArrayOutputStream();

    final int count = IOUtils.copy(in, out, buf);

    assertEquals("seed == " + seed, expected.length, count);
    assertArrayEquals("seed == " + seed, expected, out.toByteArray());
  }
}
