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

package VASSAL.tools.image;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class PNGDecoderTest {
  @Test
  public void testDecodeSignatureOk() throws IOException {
    final byte[] sig = {
      (byte) 0x89, (byte) 0x50, (byte) 0x4e, (byte) 0x47,
      (byte) 0x0d, (byte) 0x0a, (byte) 0x1a, (byte) 0x0a
    };

    final DataInputStream in =
      new DataInputStream(new ByteArrayInputStream(sig));

    assertTrue(PNGDecoder.decodeSignature(in));
  }

  @Test
  public void testDecodeSignatureWrong() throws IOException {
    final byte[] sig = new byte[8];
    Arrays.fill(sig, (byte) 0);

    final DataInputStream in =
      new DataInputStream(new ByteArrayInputStream(sig));

    assertFalse(PNGDecoder.decodeSignature(in));
  }

  @Test
  public void testDecodeSignatureShort() throws IOException {
    final DataInputStream in =
      new DataInputStream(new ByteArrayInputStream(new byte[0]));

    assertThrows(IOException.class, () -> PNGDecoder.decodeSignature(in));
  }

  @Test
  public void testDecodeChunkOk() throws IOException {
    final byte[] chunk = {
      (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x01, // length = 1
      (byte) 0x62, (byte) 0x4b, (byte) 0x47, (byte) 0x44, // bKGD
      (byte) 0x01,                                        // data
      (byte) 0xDE, (byte) 0xAD, (byte) 0xBE, (byte) 0xEF  // (bogus) CRC32
    };

    final DataInputStream in =
      new DataInputStream(new ByteArrayInputStream(chunk));

    final PNGDecoder.Chunk ch = PNGDecoder.decodeChunk(in);

    assertEquals(PNGDecoder.bKGD, ch.type);
    assertArrayEquals(new byte[] { (byte) 1 }, ch.data);
    assertEquals(0xDEADBEEFL, ch.crc);
  }

  @Test
  public void testDecodeChunkBadLength() {
    final byte[] chunk = {
      (byte) 0xff, (byte) 0xff, (byte) 0xff, (byte) 0xff
    };

    final DataInputStream in = new DataInputStream(new ByteArrayInputStream(chunk));

    assertThrows(IOException.class, () -> PNGDecoder.decodeChunk(in));
  }

  @Test
  public void testDecodeChunkShort() throws IOException {
    final byte[] chunk = {
      (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0xff,
      (byte) 0x49, (byte) 0x45, (byte) 0x4e, (byte) 0x44
    };

    final DataInputStream in =
      new DataInputStream(new ByteArrayInputStream(chunk));

    assertThrows(IOException.class, () -> PNGDecoder.decodeChunk(in));
  }
}
