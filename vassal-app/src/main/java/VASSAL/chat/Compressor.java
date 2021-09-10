/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.chat;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

public class Compressor {
  private Compressor() {
    // Helper class - not to be instantiated alone.
  }

  public static byte[] compress(byte[] in) throws IOException {
    final ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
    try (ZipOutputStream zipOut = new ZipOutputStream(byteOut)) {
      zipOut.putNextEntry(new ZipEntry("Dummy")); //$NON-NLS-1$
      zipOut.write(in);
    }
    return byteOut.toByteArray();
  }

  public static byte[] decompress(byte[] in) throws IOException {
    try (ZipInputStream zipIn = new ZipInputStream(new ByteArrayInputStream(in))) {
      zipIn.getNextEntry();
      return zipIn.readAllBytes();
    }
  }
}
