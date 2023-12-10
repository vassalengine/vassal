/*
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

import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.List;
import java.util.zip.CRC32;

/**
 * Some general purpose CRC utilities.
 *
 * @author Brent Easton
 * @since 3.1.0
 */
public class CRCUtils {

  public static final int DEFAULT_BUFFER_SIZE = 8192;
  private static final org.slf4j.Logger log = LoggerFactory.getLogger(CRCUtils.class);

  /**
   * Calculate a cumulative CRC over a series of files
   *
   * NOTE: It is up to the calling routine to ensure that the order of
   * Files in the list is consistent across all platforms.
   *
   * @param files List of files
   * @return CRC
   */
  public static long getCRC(List<File> files) {
    final CRC32 crc = new CRC32();
    final byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
    for (final File file : files) {
      try {
        buildCRC(file, crc, buffer);
      }
      catch (IOException e) {
        log.error("Error reading file " + file.getAbsolutePath() + " to generate CRC: " + e.getMessage()); // NON-NLS
        return 0L;
      }
    }
    return crc.getValue();
  }

  /**
   * Internal routine to accumulate a CRC over a single file
   *
   * @param file File
   * @param crc CRC32 to accumulate
   * @param buffer read buffer
   * @throws IOException oops
   */
  private static void buildCRC(File file, CRC32 crc, byte[] buffer) throws IOException {
    try (InputStream in = Files.newInputStream(file.toPath())) {
      buildCRC(in, crc, buffer);
    }
  }

  /**
   * Internal routine to accumulate a CRC over a single InputStream
   *
   * @param in InputStream
   * @param crc CRC32 to accumulate
   * @param buffer read buffer
   * @throws IOException oops
   */
  private static void buildCRC(InputStream in, CRC32 crc, byte[] buffer) throws IOException {

    int count;
    while ((count = in.read(buffer)) >= 0) {
      crc.update(buffer, 0, count);
    }
  }

}
