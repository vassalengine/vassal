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
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Random;

/**
 * A {@link FilterOutputStream} which handles simple obfuscation of a file's
 * contents, to prevent the casual cheat of hand-editing.
 *
 * <p>The output consists of {@link #HEADER_BYTES}, followed by the one-byte
 * key, followed by the input XORed byte-by-byte with the key.</p>
 *
 * @author uckelman
 * @since 3.2.0
 */
public class ObfuscatingOutputStream extends FilterOutputStream {
  /**
   * The header of the hex-encoded format written before VASSAL 3.8.
   *
   * @deprecated The hex-encoded format is no longer written, only read.
   * Obfuscated output is now marked with {@link #HEADER_BYTES}.
   */
  @Deprecated(since = "2026-09-08", forRemoval = true)
  public static final String HEADER = "!VCSK"; //NON-NLS

  /** The header marking obfuscated output. */
  public static final byte[] HEADER_BYTES = { '!', 'V', 'O', 'B', 'S' };

  private static final Random rand = new Random();

  private final byte key;

  /**
   * @param out the stream to wrap
   * @throws IOException oops
   */
  public ObfuscatingOutputStream(OutputStream out) throws IOException {
    // Keys are in 1-255; XORing with 0 would leave the data in plain text.
    this(out, (byte) (rand.nextInt(255) + 1));
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

    out.write(HEADER_BYTES);
    out.write(key);
  }

  /** {@inheritDoc} */
  @Override
  public void write(byte[] bytes, int off, int len) throws IOException {
    for (int i = 0; i < len; ++i) write(bytes[off + i]);
  }

  /** {@inheritDoc} */
  @Override
  public void write(int b) throws IOException {
    out.write(b ^ key);
  }

  public static void main(String[] args) throws IOException {
    try (InputStream in = args.length > 0 ? Files.newInputStream(Path.of(args[0])) : System.in;
         OutputStream out = new ObfuscatingOutputStream(new BufferedOutputStream(System.out))) {
      in.transferTo(out);
    }

    System.exit(0);
  }
}
