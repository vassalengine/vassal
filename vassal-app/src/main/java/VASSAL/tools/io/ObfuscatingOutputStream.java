/*
 *
 * Copyright (c) 2008-2026 by Joel Uckelman
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

import org.tukaani.xz.LZMA2Options;
import org.tukaani.xz.XZOutputStream;

/**
 * Obfuscates a stream of bytes: a header naming the format, a one-byte key,
 * then the data XORed with the key. Since VASSAL 3.8 the data is compressed
 * with XZ (LZMA2) before it is XORed, and the header is {@link #XZ_HEADER_BYTES}.
 *
 * <p>The obfuscation is a deterrent to casual editing of saved games, not
 * security. The compression is what a saved game's command log needs: it
 * consists of thousands of pieces whose text repeats the same prototype
 * traits, but each piece is longer than the 32 KB window of the ZIP's own
 * deflate, so deflate stores the repeat every time. LZMA2 with a 4 MB
 * dictionary ({@link #PRESET}) sees dozens of pieces at once and stores it
 * once; on a large game the stored entry is twenty times smaller, and the
 * compression is faster than deflate at level 9.</p>
 *
 * <p>{@link DeobfuscatingInputStream} reads this format and every earlier
 * one.</p>
 */
public class ObfuscatingOutputStream extends FilterOutputStream {
  /**
   * The header of the hex-encoded format written before VASSAL 3.8.
   *
   * @deprecated The hex-encoded format is no longer written, only read.
   * Obfuscated output is now marked with {@link #XZ_HEADER_BYTES}.
   */
  @Deprecated(since = "2026-09-08", forRemoval = true)
  public static final String HEADER = "!VCSK"; //NON-NLS

  /**
   * The header of the uncompressed format written by VASSAL 3.8 before the
   * command log was compressed: the key, then the data XORed with it.
   * It is still read, no longer written.
   */
  public static final byte[] HEADER_BYTES = { '!', 'V', 'O', 'B', 'S' };

  /** The header marking obfuscated output: the key, then XZ-compressed data XORed with it. */
  public static final byte[] XZ_HEADER_BYTES = { '!', 'V', 'O', 'X', 'Z' };

  /**
   * The LZMA2 preset: a 4 MB dictionary, which spans the repeated text of
   * many pieces, with the fast match finder. The higher presets gain a
   * further 20-30% for ten times the compression time.
   */
  public static final int PRESET = 3;

  private static final Random rand = new Random();

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
    super(null);

    out.write(XZ_HEADER_BYTES);
    out.write(key);

    // Everything written from here on is compressed, then XORed.
    this.out = new XZOutputStream(new XorOutputStream(out, key), new LZMA2Options(PRESET));
  }

  /** {@inheritDoc} */
  @Override
  public void write(byte[] bytes, int off, int len) throws IOException {
    out.write(bytes, off, len);
  }

  /** {@inheritDoc} */
  @Override
  public void write(int b) throws IOException {
    out.write(b);
  }

  /** XORs every byte with the key on its way out. */
  static final class XorOutputStream extends FilterOutputStream {
    private final byte key;

    XorOutputStream(OutputStream out, byte key) {
      super(out);
      this.key = key;
    }

    @Override
    public void write(byte[] bytes, int off, int len) throws IOException {
      final byte[] buf = new byte[len];
      for (int i = 0; i < len; ++i) {
        buf[i] = (byte) (bytes[off + i] ^ key);
      }
      out.write(buf, 0, len);
    }

    @Override
    public void write(int b) throws IOException {
      out.write(b ^ key);
    }
  }

  public static void main(String[] args) throws IOException {
    try (InputStream in = args.length > 0 ? Files.newInputStream(Path.of(args[0])) : System.in;
         OutputStream out = new ObfuscatingOutputStream(new BufferedOutputStream(System.out))) {
      in.transferTo(out);
    }

    System.exit(0);
  }
}
