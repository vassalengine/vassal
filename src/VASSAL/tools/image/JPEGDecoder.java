/*
 * $Id$
 *
 * Copyright (c) 2013 by Joel Uckelman
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
import java.io.FileInputStream;
import java.io.IOException;

import VASSAL.tools.io.IOUtils;

/**
 * A (partial) JPEG decoder.
 *
 * @author Joel Uckelman
 * @since 3.2.3
 */
class JPEGDecoder {
  protected JPEGDecoder() {}

  static final int TEM   = 0xFF01;
  static final int SOF0  = 0xFFC0;
  static final int SOF1  = 0xFFC1;
  static final int SOF2  = 0xFFC2;
  static final int SOF3  = 0xFFC3;
  static final int SOF4  = 0xFFC4;
  static final int SOF5  = 0xFFC5;
  static final int SOF6  = 0xFFC6;
  static final int SOF7  = 0xFFC7;
  static final int SOF8  = 0xFFC8;
  static final int SOF9  = 0xFFC9;
  static final int SOF10 = 0xFFCA;
  static final int SOF11 = 0xFFCB;
  static final int SOF12 = 0xFFCC;
  static final int SOF13 = 0xFFCD;
  static final int SOF14 = 0xFFCE;
  static final int SOF15 = 0xFFCF;
  static final int RST0  = 0xFFD0;
  static final int RST1  = 0xFFD1;
  static final int RST2  = 0xFFD2;
  static final int RST3  = 0xFFD3;
  static final int RST4  = 0xFFD4;
  static final int RST5  = 0xFFD5;
  static final int RST6  = 0xFFD6;
  static final int RST7  = 0xFFD7;
  static final int SOI   = 0xFFD8;
  static final int EOI   = 0xFFD9;
  static final int SOS   = 0xFFDA;
  static final int APP0  = 0xFFE0;
  static final int APP2  = 0xFFE2;
  static final int APP13 = 0xFFED;
  static final int APP14 = 0xFFEE;

  public static boolean decodeSignature(DataInputStream in) throws IOException {
    return in.readUnsignedShort() == SOI; 
  }

  public static Chunk decodeChunk(DataInputStream in) throws IOException {
    final int type = in.readUnsignedShort(); // marker type

    if ((type & 0xFF00) != 0xFF00) {
      throw new IOException("initial byte of chunk must be FF");
    }

    byte[] data;

    if (type == TEM || (RST0 <= type && type <= EOI)) {
      // These chunks have no data
      data = new byte[0];
    }
    else {
      final int length = in.readUnsignedShort();
      data = new byte[length-2]; // length is inclusive of its own 2 bytes
      in.readFully(data);
    }

    // NB: This will blow up after reaching an SOS, due to it being follwed
    // by raw data instead of another chunk. If we want to find the next
    // chunk after an SOS, we have to scan for FF xx, where xx != 00.

    return new Chunk(type, data);
  }

  public static class Chunk {
    public final int type;
    public final byte[] data;

    private Chunk(int type, byte[] data) {
      this.type = type;
      this.data = data;
    }
  }

  public static void main(String[] args) throws IOException {
    DataInputStream in = null;
    try {
      in = new DataInputStream(new FileInputStream(args[0]));
      
      if (!JPEGDecoder.decodeSignature(in)) {
        System.out.println("Not a JPEG");
      }

      JPEGDecoder.Chunk ch;
      do {
        ch = JPEGDecoder.decodeChunk(in);
	      System.out.println("type == " + Integer.toHexString(ch.type) + ", length == " + ch.data.length);


      } while (ch.type != JPEGDecoder.EOI);
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }
}
