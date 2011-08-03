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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools.io;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Random;

/**
 * A {@link FilterOutputStream} which handles simple obfuscation of a file's
 * contents, to prevent the casual cheat of hand-editing.
 *
 * @author uckelman
 * @since 3.2.0
 */
public class ObfuscatingOutputStream extends FilterOutputStream {
  public static final String HEADER = "!VCSK";
  private static final Random rand = new Random();

  private final byte key;
  private final byte[] pair = new byte[2];

  /**
   * @param out the stream to wrap
   * @throws IOException
   */
  public ObfuscatingOutputStream(OutputStream out) throws IOException {
    this(out, (byte) rand.nextInt(256));
  }

  /**
   * @param out the stream to wrap
   * @param key the byte to use as the key
   * @throws IOException
   */
  public ObfuscatingOutputStream(OutputStream out, byte key)
                                                          throws IOException {
    super(out);
    this.key = key;

    out.write(HEADER.getBytes("UTF-8"));

    pair[0] = hex[(key & 0xF0) >>> 4];
    pair[1] = hex[key & 0x0F];
    out.write(pair);
  }

  /** {@inheritDoc} */
  @Override
  public void write(byte[] bytes, int off, int len) throws IOException {
    for (int i = 0; i < len; ++i) write(bytes[off+i]);
  }

  private final static byte[] hex = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  };

  /** {@inheritDoc} */
  @Override
  public void write(int b) throws IOException {
    b ^= key;

    pair[0] = hex[(b & 0xF0) >>> 4];
    pair[1] = hex[b & 0x0F];
    out.write(pair);
  }

  public static void main(String[] args) throws IOException {
    InputStream in = null;
    OutputStream out = null;
    try {
      in = args.length > 0 ? new FileInputStream(args[0]) : System.in;
      out = new ObfuscatingOutputStream(new BufferedOutputStream(System.out));
      IOUtils.copy(in, out);
      out.close();
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    System.exit(0);
  }
}
