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

import VASSAL.build.module.GameState;

/**
 * Formerly obfuscated a saved game's command log by XORing it with a key,
 * so that it was not plain text inside the ZIP. The log is now compressed
 * with XZ instead, which is not plain text either, and this class only
 * delegates to that.
 *
 * @deprecated Use {@link GameState#compressSavedGame(OutputStream)}.
 * {@link DeobfuscatingInputStream} still reads the formats this class wrote.
 */
@Deprecated(since = "2026-10-02", forRemoval = true)
public class ObfuscatingOutputStream extends FilterOutputStream {
  /**
   * The header of the hex-encoded format written before VASSAL 3.8.
   *
   * @deprecated The hex-encoded format is no longer written, only read.
   */
  @Deprecated(since = "2026-09-08", forRemoval = true)
  public static final String HEADER = "!VCSK"; //NON-NLS

  /**
   * @param out the stream to wrap
   * @throws IOException oops
   */
  public ObfuscatingOutputStream(OutputStream out) throws IOException {
    super(GameState.compressSavedGame(out));
  }

  /**
   * @param out the stream to wrap
   * @param key ignored; nothing is XORed any more
   * @throws IOException oops
   */
  public ObfuscatingOutputStream(OutputStream out, byte key) throws IOException {
    this(out);
  }

  /** {@inheritDoc} */
  @Override
  public void write(byte[] bytes, int off, int len) throws IOException {
    out.write(bytes, off, len);
  }

  public static void main(String[] args) throws IOException {
    try (InputStream in = args.length > 0 ? Files.newInputStream(Path.of(args[0])) : System.in;
         OutputStream out = GameState.compressSavedGame(new BufferedOutputStream(System.out))) {
      in.transferTo(out);
    }

    System.exit(0);
  }
}
