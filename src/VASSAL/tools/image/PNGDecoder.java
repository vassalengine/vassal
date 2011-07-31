/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

import java.io.DataInputStream;
import java.io.IOException;

/**
 * A (partial) PNG decoder.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
class PNGDecoder {
  protected PNGDecoder() {}

  // critical chunks
  static final int IHDR = 0x49484452;
  static final int PLTE = 0x504c5445;
  static final int IDAT = 0x49444154;
  static final int IEND = 0x49454e44;

  // ancillary chunks
  static final int bKGD = 0x624b4744;
  static final int cHRM = 0x6348524d;
  static final int gAMA = 0x67414d41;
  static final int hIST = 0x68495354;
  static final int iCCP = 0x69434350;
  static final int iTXt = 0x69545874;
  static final int pHYs = 0x70485973;
  static final int sBIT = 0x73424954;
  static final int sPLT = 0x73504c54;
  static final int sRGB = 0x73524742;
  static final int tEXt = 0x74455874;
  static final int tIME = 0x74494d45;
  static final int tRNS = 0x74524e53;
  static final int zTXt = 0x7a545874;

  static final long sig = 0x89504e470d0a1a0aL;

  public static boolean decodeSignature(DataInputStream in)
                                                          throws IOException {
    // 5.2
    return in.readLong() == sig;
  }

  public static Chunk decodeChunk(DataInputStream in) throws IOException {
    // 5.3
    final int length = in.readInt();

    // Length should not exceed 2^31-1. Since ints are signed, any value
    // greater than 2^31-1 will look negative to us.
    if (length < 0) {
      throw new IOException("chunk length out of range");
    }

    final int type = in.readInt();

    final byte[] data = new byte[length];
    in.readFully(data);

    final long crc = in.readInt() & 0x0000000ffffffffL;

/*
    final CRC32 crc32 = new CRC32();
    crc32.update(type);
    crc32.update(data);
    if (crc != crc32.getValue())
      throw new IOException("corrupted " + type + " chunk");
*/

    return new Chunk(type, data, crc);
  }

  public static class Chunk {
    public final int type;
    public final byte[] data;
    public final long crc;

    private Chunk(int type, byte[] data, long crc) {
      this.type = type;
      this.data = data;
      this.crc  = crc;
    }
  }
}
