/*
 * Copyright (c) 2021 by The VASSAL Development Team
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
 *
 * Attribution:
 * This code is a pre-release patch for the equivalent, currently brain-dead Apache Commons methods.
 * Adapted for Vassal coding standards.
 * See https://issues.apache.org/jira/browse/IO-373
 */
package VASSAL.tools.io;

import static org.apache.commons.io.FileUtils.ONE_EB_BI;
import static org.apache.commons.io.FileUtils.ONE_GB_BI;
import static org.apache.commons.io.FileUtils.ONE_KB_BI;
import static org.apache.commons.io.FileUtils.ONE_MB_BI;
import static org.apache.commons.io.FileUtils.ONE_PB_BI;
import static org.apache.commons.io.FileUtils.ONE_TB_BI;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Custom Implementation of FileUtils.byteCountToDisplaySize to fix rounding bug
 */
public class FileUtils {

  private static final RoundingMode ROUNDING_MODE = RoundingMode.HALF_UP;

  enum FileSize {
    EXABYTE("EB", ONE_EB_BI),
    PETABYTE("PB", ONE_PB_BI),
    TERABYTE("TB", ONE_TB_BI),
    GIGABYTE("GB", ONE_GB_BI),
    MEGABYTE("MB", ONE_MB_BI),
    KILOBYTE("KB", ONE_KB_BI),
    BYTE("bytes", BigInteger.ONE); // NON-NLS

    private final String unit;
    private final BigInteger byteCount;

    FileSize(String unit, BigInteger byteCount) {
      this.unit = unit;
      this.byteCount = byteCount;
    }
  }

  /**
   * Formats a file's size into a human readable format
   *
   * @param fileSize the file's size as BigInteger
   * @return the size as human readable string
   */
  public static String byteCountToDisplaySize(final BigInteger fileSize) {

    String unit = FileSize.BYTE.unit;
    BigDecimal fileSizeInUnit = BigDecimal.ZERO;
    String val;

    for (final FileSize fs : FileSize.values()) {
      final BigDecimal size_bd = new BigDecimal(fileSize);
      fileSizeInUnit = size_bd.divide(new BigDecimal(fs.byteCount), 5, ROUNDING_MODE);
      if (fileSizeInUnit.compareTo(BigDecimal.ONE) >= 0) {
        unit = fs.unit;
        break;
      }
    }

    // always round so that at least 3 numerics are displayed (###, ##.#, #.##)
    if (fileSizeInUnit.divide(BigDecimal.valueOf(100.0), RoundingMode.DOWN).compareTo(BigDecimal.ONE) >= 0) {
      val = fileSizeInUnit.setScale(0, ROUNDING_MODE).toString();
    }
    else if (fileSizeInUnit.divide(BigDecimal.valueOf(10.0), RoundingMode.DOWN).compareTo(BigDecimal.ONE) >= 0) {
      val = fileSizeInUnit.setScale(1, ROUNDING_MODE).toString();
    }
    else {
      val = fileSizeInUnit.setScale(2, ROUNDING_MODE).toString();
    }

    // trim zeros at the end
    if (val.endsWith(".00")) {
      val = val.substring(0, val.length() - 3);
    }
    else if (val.endsWith(".0")) {
      val = val.substring(0, val.length() - 2);
    }

    return String.format("%s %s", val, unit); // NON-NLS
  }


  /**
   * Formats a file's size into a human readable format
   *
   * @param fileSize the file's size as long
   * @return the size as human readable string
   */
  public static String byteCountToDisplaySize(final long fileSize) {
    return byteCountToDisplaySize(BigInteger.valueOf(fileSize));
  }

}
