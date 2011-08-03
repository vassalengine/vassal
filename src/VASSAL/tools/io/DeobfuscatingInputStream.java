/*
 * $Id$
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Joel Uckelman
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

import java.io.FileInputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;

/**
 * A {@link FilterInputStream} which converts a file created with
 * {@link ObfuscatingOutputStream} back into plain text.
 * Additionally, plain text will be passed through unchanged.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class DeobfuscatingInputStream extends FilterInputStream {

  /**
   * @param in the stream to wrap
   * @throws IOException
   */
  public DeobfuscatingInputStream(InputStream in) throws IOException {
    super(null);

    final byte[] header = new byte[ObfuscatingOutputStream.HEADER.length()];
    readFully(in, header, 0, header.length);
    if (new String(header, "UTF-8").equals(ObfuscatingOutputStream.HEADER)) {
      this.in = new DeobfuscatingInputStreamImpl(in);
    }
    else {
      final PushbackInputStream pin =
        new PushbackInputStream(in, header.length);
      pin.unread(header);
      this.in = pin;
    }
  }

  /**
   * Reads the given number of bytes.
   *
   * @param in the source
   * @param bytes the destination
   * @param off the offset into the destination array
   * @param len the number of bytes to read
   * @throws IOException if <code>len</code> bytes cannot be read
   */
  private static int readFully(InputStream in, byte[] bytes, int off, int len)
                                                           throws IOException {
    int count;
    int n = 0;
    while (n < len) {
      count = in.read(bytes, off + n, len - n);
      if (count < 0) break;
      n += count;
    }

    return n;
  }

  private static class DeobfuscatingInputStreamImpl extends FilterInputStream {
    private final byte key;
    private final byte[] pair = new byte[2];

    public DeobfuscatingInputStreamImpl(InputStream in) throws IOException {
      super(in);

      readFully(in, pair, 0, 2);
      key = (byte) ((unhex(pair[0]) << 4) | unhex(pair[1]));
    }

    @Override
    public int read(byte[] bytes, int off, int len) throws IOException {
      int b = 0;
      int i = 0;
      while (i < len && (b = read()) >= 0) bytes[(i++)+off] = (byte) b;
      return b == -1 && i == 0 ? -1 : i;
    }

    @Override
    public int read() throws IOException {
      switch (readFully(in, pair, 0, 2)) {
      case  0:
        return -1;
      case  1:
        throw new IOException();
      case  2:
        return (((unhex(pair[0]) << 4) | unhex(pair[1])) ^ key) & 0xFF;
      default:
        throw new IOException();
      }
    }

    private int unhex(int i) throws IOException {
      switch (i) {
      // digits 0-9
      case 0x30:
      case 0x31:
      case 0x32:
      case 0x33:
      case 0x34:
      case 0x35:
      case 0x36:
      case 0x37:
      case 0x38:
      case 0x39:
        return i - 0x30;
      // digits A-F
      case 0x41:
      case 0x42:
      case 0x43:
      case 0x44:
      case 0x45:
      case 0x46:
        return i - 0x37;
      // digits a-f
      case 0x61:
      case 0x62:
      case 0x63:
      case 0x64:
      case 0x65:
      case 0x66:
        return i - 0x57;
      default:
        throw new IOException(String.valueOf(i));
      }
    }
  }

  public static void main(String[] args) throws IOException {
    InputStream in = null;
    try {
      in = new DeobfuscatingInputStream(
        args.length > 0 ? new FileInputStream(args[0]) : System.in);
      IOUtils.copy(in, System.out);
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    System.exit(0);
  }
}
