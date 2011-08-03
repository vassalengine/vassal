/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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
package VASSAL.tools;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Random;

import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.ObfuscatingOutputStream;

/**
 * Utility class that handles simple obfuscation of a file's contents,
 * to prevent the casual cheat of hand-editing a logfile.
 *
 * @author rkinney
 * @deprecated Use {@link ObfuscatingOutputStream} instead.
 */
@Deprecated
public class Obfuscator {
  public static final String HEADER = "!VCSK";
  private static final Random rand = new Random();
  private String encrypted;
  private byte key;

  public Obfuscator(byte[] contents) {
    key = (byte) rand.nextInt(256);
    final StringBuilder buffer = new StringBuilder(HEADER);
    appendAsHex(buffer,key);
    for (int i = 0; i < contents.length; ++i) {
      appendAsHex(buffer,(byte) (contents[i] ^ key));
    }
    encrypted = buffer.toString();
  }

  private void appendAsHex(StringBuilder buffer, byte b) {
    buffer.append(Integer.toHexString((b & 0xf0) >>> 4).charAt(0));
    buffer.append(Integer.toHexString(b & 0x0f).charAt(0));
  }

  public void write(OutputStream out) throws IOException {
    out.write(encrypted.getBytes("UTF-8"));
  }

  // Convert a plain text file to an obfuscated file
  public static void main(String[] args) throws Exception {
    final InputStream in =
      args.length > 0 ? new FileInputStream(args[0]) : System.in;
    final byte[] data;
    try {
      data = IOUtils.toByteArray(in);
    }
    finally {
      in.close();
    }

    Obfuscator o = new Obfuscator(data);
    o.write(new BufferedOutputStream(System.out));
    System.exit(0);
  }
}
