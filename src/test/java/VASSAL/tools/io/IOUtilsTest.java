/*
 * $Id$
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
import java.io.InputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Random;
import java.util.zip.ZipFile;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.MemoryCacheImageInputStream;

import org.apache.commons.io.input.ClosedInputStream;
import org.apache.commons.io.input.NullInputStream;
import org.apache.commons.io.FileUtils;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.*;

@RunWith(JMock.class)
public class IOUtilsTest {
  protected final Mockery context = new JUnit4Mockery();

  @Test
  public void testCopyFileChannels() throws IOException {
    final File ifile = new File("test/VASSAL/tools/io/IOUtilsTest.java");
    final File ofile = new File("test/VASSAL/tools/io/test.out");

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
    final File ifile = new File("test/VASSAL/tools/io/IOUtilsTest.java");
    final File ofile = new File("test/VASSAL/tools/io/test.out");

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

  @Test
  public void testCopyLargeBuffer() throws IOException {
    final byte[] buf = new byte[1024];

    final byte[] expected = new byte[10000];
    final long seed = System.currentTimeMillis();
    final Random rng = new Random(seed);
    rng.nextBytes(expected);

    final ByteArrayInputStream in = new ByteArrayInputStream(expected);
    final ByteArrayOutputStream out = new ByteArrayOutputStream();

    final long count = IOUtils.copyLarge(in, out, buf);

    assertEquals("seed == " + seed, expected.length, count);
    assertArrayEquals("seed == " + seed, expected, out.toByteArray());
  }

  @Test
  public void testCloseQuietlyZipFileOpen() throws IOException {
    final ZipFile zf = new ZipFile("test/VASSAL/tools/io/test.zip");
    IOUtils.closeQuietly(zf);

    try {
      zf.size();
      fail();
    }
    catch (IllegalStateException e) {
      // This is the expected behavior of size().
    }
  }

  @Test
  public void testCloseQuietlyZipFileClosed() throws IOException {
    final ZipFile zf = new ZipFile("test/VASSAL/tools/io/test.zip");
    zf.close();
    IOUtils.closeQuietly(zf);
  }

  @Test
  public void testCloseQuietlyZipFileNull() {
    IOUtils.closeQuietly((ZipFile) null);
  }

  @Test
  public void testCloseQuietlyImageInputStreamOpen() {
    final ImageInputStream in =
      new MemoryCacheImageInputStream(new NullInputStream(1000));

    IOUtils.closeQuietly(in);

    try {
      in.close();
      fail();
    }
    catch (IOException e) {
      // This, oddly, the expected behavior of close().
    }
  }

  @Test
  public void testCloseQuietlyImageInputStreamClosed() throws IOException {
    final ImageInputStream in =
      new MemoryCacheImageInputStream(new ClosedInputStream());
    in.close();

    IOUtils.closeQuietly(in);
  }

  @Test
  public void testCloseQuietlyImageInputStreamNull() {
    IOUtils.closeQuietly((ImageInputStream) null);
  }
}
