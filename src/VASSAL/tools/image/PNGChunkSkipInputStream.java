/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman
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
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Set;

class PNGChunkSkipInputStream extends InputStream {

  public PNGChunkSkipInputStream(Set<Integer> skip, InputStream in)
                                                           throws IOException {
    this.skip = skip;
    this.in = new DataInputStream(in);

    if (!PNGDecoder.decodeSignature(this.in)) throw new IOException();

    bout = new ByteArrayOutputStream();
    out = new DataOutputStream(bout);

    out.writeLong(PNGDecoder.sig);
    flipBuffer();
  }

  protected final Set<Integer> skip;

  protected final DataInputStream in;

  protected ByteArrayInputStream bin;
  protected final ByteArrayOutputStream bout;
  protected final DataOutputStream out;

  protected boolean seenIEND = false;

  public int available() throws IOException {
    return bin.available();
  }

  public void close() throws IOException {
    in.close();
  }

  public void mark(int readlimit) {}

  public boolean markSupported() {
    return false;
  }

  public int read() throws IOException {
    final byte[] b = new byte[1];
    return read(b) == -1 ? -1 : b[0];
  }

  public int read(byte[] b) throws IOException {
    return read(b, 0, b.length);
  }

  public int read(byte[] b, int off, int len) throws IOException {
    int ret = bin.read(b, off, len);

    if (ret == -1 && !seenIEND) {
      PNGDecoder.Chunk ch;

      do {
        ch = PNGDecoder.decodeChunk(in);
        if (ch.type == PNGDecoder.IEND) seenIEND = true;
      } while (skip.contains(ch.type));

      encodeChunk(ch);
      flipBuffer();

      ret = bin.read(b, off, len);
    }

    return ret;
  }

  protected void encodeChunk(PNGDecoder.Chunk ch) throws IOException {
    // write chunk data length
    out.writeInt(ch.data.length);

    // write chunk type
    out.writeInt(ch.type);

    // write data
    out.write(ch.data);

    // write checksum
    out.writeInt((int) ch.crc);
  }

  protected void flipBuffer() throws IOException {
    out.flush();
    bin = new ByteArrayInputStream(bout.toByteArray());
    bout.reset();
  }

  public void reset() throws IOException {
    throw new IOException();
  }

  public long skip(long n) throws IOException {
    throw new IOException();
  }

/*
  public static void main(String[] args) throws Exception {

    final InputStream fin = new PNGChunkSkipInputStream(
      java.util.Collections.singleton(PNGDecoder.iTXt),
      new java.io.FileInputStream("iTXt.png")
    );

    VASSAL.tools.io.IOUtils.copy(fin, System.out);
    fin.close();
  }
*/
}
