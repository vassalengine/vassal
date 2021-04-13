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

package VASSAL.tools.image.tilecache;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.zip.GZIPOutputStream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static VASSAL.tools.image.AssertImage.*;

public class TileUtilsTest {

  private static byte[] bytes;
  private static BufferedImage src;

  @BeforeEach
  public void setup() throws IOException {
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();

    // write the header
    final DataOutputStream hout = new DataOutputStream(baos);
    hout.write("VASSAL".getBytes());
    hout.writeInt(1);
    hout.writeInt(1);
    hout.writeInt(BufferedImage.TYPE_INT_ARGB);
    hout.close();

    // write the compressed data
    final DataOutputStream zout =
      new DataOutputStream(new GZIPOutputStream(baos));
    zout.writeInt(0xDEADBEEF);
    zout.close();

    bytes = baos.toByteArray();

    // the same 1x1 test image as a BufferedImage
    src = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);
    src.setRGB(0, 0, 0xDEADBEEF);
  }

  @Test
  public void testReadInputStreamOk() throws IOException {
    final InputStream in = new ByteArrayInputStream(bytes);
    final BufferedImage img = TileUtils.read(in);
    assertImageEquals(src, img);
  }

  @Test
  public void testReadInputStreamUnderflow() {
    final InputStream in = new ByteArrayInputStream(new byte[0]);
    assertThrows(IOException.class, () -> TileUtils.read(in));
  }

/*
  @Test(expected=IOException.class)
  public void testReadInputStreamOverflow() throws IOException {
    // a 1x1 test image with trailing garbage
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    baos.write(bytes);
    baos.write("garbage".getBytes());

    final InputStream in = new ByteArrayInputStream(baos.toByteArray());
    TileUtils.read(in);
  }
*/

  @Test
  public void testReadInputStreamBadSignature() {
    final InputStream in = new ByteArrayInputStream("xyzzy".getBytes());
    assertThrows(IOException.class, () -> TileUtils.read(in));
  }

  @Test
  public void testReadHeaderOk() throws IOException {
    final InputStream in = new ByteArrayInputStream(bytes);
    final byte[] actual = TileUtils.readHeader(in);
    assertArrayEquals(Arrays.copyOfRange(bytes, 0, 18), actual);
  }

  @Test
  public void testReadHeaderBad() {
    final InputStream in = new ByteArrayInputStream("xyzzy".getBytes());
    assertThrows(IOException.class, () -> TileUtils.readHeader(in));
  }

  @Test
  public void testCheckSignatureOk() throws IOException {
    TileUtils.checkSignature("VASSAL".getBytes());
  }

  @Test
  public void testCheckSignatureBad()  {
    assertThrows(IOException.class, () -> TileUtils.checkSignature("xyzzy".getBytes()));
  }

  @Test
  public void testSizeOk() throws IOException {
    final InputStream in = new ByteArrayInputStream(bytes);
    final Dimension d = TileUtils.size(in);
    assertEquals(new Dimension(src.getWidth(), src.getHeight()), d);
  }

  @Test
  public void testSizeUnderflow() {
    final InputStream in =
      new ByteArrayInputStream(Arrays.copyOfRange(bytes, 0, 12));
    assertThrows(IOException.class, () -> TileUtils.size(in));
  }

  @Test
  public void testWriteOutputStream() throws IOException {
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    TileUtils.write(src, out);
    assertArrayEquals(bytes, out.toByteArray());
  }

  @Test
  public void testWriteReadRoundTrip() throws IOException {
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    TileUtils.write(src, out);

    final InputStream in = new ByteArrayInputStream(out.toByteArray());
    final BufferedImage dst = TileUtils.read(in);

    assertImageEquals(src, dst);
  }

  @Test
  public void testReadWriteRoundTrip() throws IOException {
    final InputStream in = new ByteArrayInputStream(bytes);
    final BufferedImage dst = TileUtils.read(in);

    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    TileUtils.write(dst, out);

    assertArrayEquals(bytes, out.toByteArray());
  }

  @Test
  public void testTileCountInt() {
    final int tests[][] = {
    // tiles  iw    ih   tw   th
      {  1,    1,    2,  10,  20 },
      {  1,    2,    1,  10,  20 },
      { 39, 1500, 1000, 256, 256 }
    };

    for (int[] t : tests) {
      assertEquals(
        t[0],
        TileUtils.tileCount(t[1], t[2], t[3], t[4])
      );
    }
  }

  @Test
  public void testTileCountAtScaleInt() {
    final int tests[][] = {
    // tiles  iw    ih   tw   th  scale
      {  1,    1,    2,  10,  20,  1 },
      {  0,    1,    2,  10,  20,  2 },
      { 24, 1500, 1000, 256, 256,  1 },
      { 24, 1000, 1500, 256, 256,  1 },
      {  6, 1500, 1000, 256, 256,  2 },
      {  2, 1500, 1000, 256, 256,  4 },
      {  1, 1500, 1000, 256, 256,  8 },
      {  1, 1500, 1000, 256, 256, 16 }
    };

    for (int[] t : tests) {
      assertEquals(
        t[0],
        TileUtils.tileCountAtScale(t[1], t[2], t[3], t[4], t[5])
      );
    }
  }

  @Test
  public void testTileCountAtScaleIntBad() {
    final int[] t = new int[5];
    for (int i = 0; i < t.length; ++i) {
      Arrays.fill(t, 1);
      t[i] = 0;

      try {
        TileUtils.tileCountAtScale(t[0], t[1], t[2], t[3], t[4]);
        fail(Arrays.toString(t));
      }
      catch (IllegalArgumentException e) {
      }
    }
  }
}
