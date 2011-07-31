/*
 * $Id$
 *
 * Copyright (c) 2009 by Brent Easton
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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.zip.CRC32;

import VASSAL.tools.io.IOUtils;

/**
 * Some general purpose CRC utilities.
 *
 * @author Brent Easton
 * @since 3.1.0
 */
public class CRCUtils {

  public static final int DEFAULT_BUFFER_SIZE = 8192;

  /**
   * Calculate a cumulative CRC over a series of files
   *
   * NOTE: It is up to the calling routine to ensure that the order of
   * Files in the list is consistent accross all platforms.
   *
   * @param files List of files
   * @return CRC
   * @throws IOException
   */
  public static long getCRC(List<File> files) throws IOException {
    final CRC32 crc = new CRC32();
    final byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
    for (File file : files) {
      buildCRC(file, crc, buffer);
    }
    return crc.getValue();
  }

  /**
   * Internal routine to accumulate a CRC over a single file
   *
   * @param file File
   * @param crc CRC32 to accumulate
   * @param bytes size of File (out)
   * @param buffer read buffer
   * @throws IOException
   */
  private static void buildCRC(File file, CRC32 crc, byte[] buffer) throws IOException {
    InputStream in = new FileInputStream(file);
    try {
      buildCRC(in, crc, buffer);
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  /**
   * Internal routine to accumulate a CRC over a single InputStream
   *
   * @param in InputStream
   * @param crc CRC32 to accumulate
   * @param bytes number of bytes read
   * @param buffer read buffer
   * @throws IOException
   */
  private static void buildCRC(InputStream in, CRC32 crc, byte[] buffer) throws IOException {

    int count;
    while ((count = in.read(buffer)) >= 0) {
      crc.update(buffer, 0, count);
    }
  }

}
