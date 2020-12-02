/*
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
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Random;

/**
 * A {@link FilterOutputStream} which handles simple obfuscation of a file's
 * contents, to prevent the casual cheat of hand-editing.
 *
 * @author uckelman
 * @since 3.2.0
 */
public class ObfuscatingOutputStream extends FilterOutputStream {
  public static final String HEADER = "!VCSK"; //NON-NLS
  private static final Random rand = new Random();

  private final byte key;
  private final byte[] pair = new byte[2];

  /**
   * @param out the stream to wrap
   * @throws IOException oops
   */
  public ObfuscatingOutputStream(OutputStream out) throws IOException {
    this(out, (byte) rand.nextInt(256));
  }

  /**
   * @param out the stream to wrap
   * @param key the byte to use as the key
   * @throws IOException oops
   */
  public ObfuscatingOutputStream(OutputStream out, byte key)
                                                          throws IOException {
    super(out);
    this.key = key;

    out.write(HEADER.getBytes(StandardCharsets.UTF_8));

    pair[0] = HEX[(key & 0xF0) >>> 4];
    pair[1] = HEX[key & 0x0F];
    out.write(pair);
  }

  /** {@inheritDoc} */
  @Override
  public void write(byte[] bytes, int off, int len) throws IOException {
    for (int i = 0; i < len; ++i) write(bytes[off + i]);
  }

  private static final byte[] HEX = {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  };

  /** {@inheritDoc} */
  @Override
  public void write(int b) throws IOException {
    b ^= key;

    pair[0] = HEX[(b & 0xF0) >>> 4];
    pair[1] = HEX[b & 0x0F];
    out.write(pair);
  }

  public static void main(String[] args) throws IOException {
    try (InputStream in = args.length > 0 ? Files.newInputStream(Path.of(args[0])) : System.in;
         OutputStream out = new ObfuscatingOutputStream(new BufferedOutputStream(System.out))) {
      in.transferTo(out);
    }

    System.exit(0);
  }
}
