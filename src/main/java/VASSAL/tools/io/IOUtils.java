/*
 *
 * Copyright (c) 2007-2010 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools.io;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.FileChannel;

/**
 * General I/O stream manipulation utilities. This class provides static
 * utility methods to reduce boilerplate I/O code.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class IOUtils extends org.apache.commons.io.IOUtils {
  protected IOUtils() {}

  /**
   * Copies bytes from a <code>FileInputStream</code> to a
   * <code>FileOutputStream</code>.
   *
   * This method uses channels. The input file should not be written
   * to during the copy.
   *
   * @param in the source
   * @param out the destination
   * @throws IOException if one occurs while reading or writing
   */
  public static int copy(FileInputStream in, FileOutputStream out)
                                                           throws IOException {
    final long count = copyLarge(in, out);
    return count > Integer.MAX_VALUE ? -1 : (int) count;
  }

  /**
   * Copies bytes from a large (over 2GB) <code>FileInputStream</code> to a
   * <code>FileOutputStream</code>.
   *
   * This method uses channels. The input file should not be written
   * to during the copy.
   *
   * @param in the source
   * @param out the destination
   * @throws IOException if one occurs while reading or writing
   */
  public static long copyLarge(FileInputStream in, FileOutputStream out)
                                                           throws IOException {
    final FileChannel inc = in.getChannel();
    return inc.transferTo(0L, inc.size(), out.getChannel());
  }

  /**
   * Copies bytes from an <code>InputStream</code> to an
   * <code>OutputStream</code> via a <code>byte</code> buffer. This
   * method buffers input internally, so the input stream should not
   * be a <code>BufferedInputStream</code>.
   *
   * @param in the source
   * @param out the destination
   * @param buffer the buffer
   * @return the number of bytes copied
   * @throws IOException if one occurs while reading or writing
   */
  public static int copy(InputStream in, OutputStream out, byte[] buffer)
                                                           throws IOException {
    final long count = copyLarge(in, out, buffer);
    return count > Integer.MAX_VALUE ? -1 : (int) count;
  }

  /**
   * Close an {@link AutoCloseable} unconditionally. Equivalent to
   * calling <code>c.close()</code> when <code>c</code> is nonnull.
   *
   * @param c a (possibly <code>null</code>) {@link AutoCloseable}
   * @deprecated use try with resources or close and catch manually
   */
  @Deprecated
  public static void closeQuietly(AutoCloseable c) {
    if (c == null) return;

    try {
      c.close();
    }
    catch (Exception e) {
      // ignore
    }
  }
}
