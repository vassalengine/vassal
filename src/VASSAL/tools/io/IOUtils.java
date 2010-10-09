/*
 * $Id$
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

import java.io.Closeable;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.channels.FileChannel;
import java.util.zip.ZipFile;
import javax.imageio.stream.ImageInputStream;

/**
 * General I/O stream manipulation utilities. This class provides static
 * utility methods to reduce boilerplate I/O code.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */ 
public class IOUtils extends org.apache.commons.io.IOUtils {
  /**
   * Copies bytes from a <code>FileInputStream</code> to a
   * <code>FileOutputStream</code>. This method uses channels.
   *
   * @param in the source
   * @param out the destination
   * @throws IOException if one occurs while reading or writing.
   */
  public static int copy(FileInputStream in, FileOutputStream out)
                                                           throws IOException {
    final FileChannel inc = in.getChannel();
    final long count = inc.transferTo(0L, inc.size(), out.getChannel());
    return count > Integer.MAX_VALUE ? -1 : (int) count;
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
   * @throws IOException if one occurs while reading or writing.
   */
  public static int copy(InputStream in, OutputStream out, byte[] buffer)
                                                           throws IOException {
    final long count = copyLarge(in, out, buffer);
    return count > Integer.MAX_VALUE ? -1 : (int) count;
  }

  /**
   * Copies bytes from a large (over 2GB) <code>InputStream</code> to an
   * <code>OutputStream</code> via a <code>byte</code> buffer. This
   * method buffers input internally, so the input stream should not
   * be a <code>BufferedInputStream</code>.
   *
   * @param in the source
   * @param out the destination
   * @param buffer the buffer
   * @return the number of bytes copied
   * @throws IOException if one occurs while reading or writing.
   */
  public static long copyLarge(InputStream in, OutputStream out, byte[] buffer)
                                                           throws IOException {
    long count = 0;
    int n = 0;
    while ((n = in.read(buffer)) != -1) {
      out.write(buffer, 0, n);
      count += n;
    }
    return count;
  }

  /**
   * Close a {@link Closeable} unconditionally. Equivalent to
   * calling <code>c.close()</code> when <code>c</code> is nonnull.
   * {@link IOException}s are swallowed, as there is generally
   * nothing that can be done about exceptions on closing.
   *
   * @param c a (possibly <code>null</code>) <code>Closeable</code>
   */
  public static void closeQuietly(Closeable c) {
    if (c == null) return;
    
    try {
      c.close();
    }
    catch (IOException e) {
      // ignore 
    }
  }

  /**
   * Close a {@link ServerSocket} unconditionally. Equivalent to
   * calling <code>s.close()</code> when <code>s</code> is nonnull.
   * {@link IOException}s are swallowed, as there is generally
   * nothing that can be done about exceptions on closing.
   *
   * @param c a (possibly <code>null</code>) <code>ServerSocket</code>
   */
  // FIXME: Remove in Java 1.6+, when ServerSocket implements Closeable 
  public static void closeQuietly(ServerSocket s) {
    if (s == null) return;

    try {
      s.close();
    }
    catch (IOException e) {
      // ignore
    }
  }

  /**
   * Close a {@link Socket} unconditionally. Equivalent to
   * calling <code>s.close()</code> when <code>s</code> is nonnull.
   * {@link IOException}s are swallowed, as there is generally
   * nothing that can be done about exceptions on closing.
   *
   * @param c a (possibly <code>null</code>) <code>Socket</code>
   */
  // FIXME: Remove in Java 1.6+, when Socket implements Closeable
  public static void closeQuietly(Socket s) {
    if (s == null) return;

    try {
      s.close();
    }
    catch (IOException e) {
      // ignore
    }
  }

  /**
   * Close a {@link ZipFile} unconditionally. Equivalent to
   * calling <code>z.close()</code> when <code>z</code> is nonnull.
   * {@link IOException}s are swallowed, as there is generally
   * nothing that can be done about exceptions on closing.
   *
   * @param z a (possibly <code>null</code>) <code>ZipFile</code>
   */
  // Why doesn't ZipFile implement Closeable? Argh!
  public static void closeQuietly(ZipFile z) {
    if (z == null) return;

    try {
      z.close();
    }
    catch (IOException e) {
      // ignore
    }
  }

  /**
   * Close an {@link ImageInputStream} unconditionally. Equivalent to
   * calling <code>s.close()</code> when <code>s</code> is nonnull.
   * {@link IOException}s are quietly logged, as there is generally
   * nothing that can be done about exceptions on closing.
   *
   * @param s a (possibly <code>null</code>) <code>ImageInputStream</code>
   */
  // Why doesn't ImageInputStream implement Closeable? Argh!
  public static void closeQuietly(ImageInputStream s) {
    if (s == null) return;

    try {
      s.close();
    }
    catch (IOException e) {
      // ignore

      // Note that ImageInputStreamImpl.close() rather ridiculously throws
      // an IOException if the stream is already closed. This is always done
      // via ImageInputStreamImpl.checkClosed().
    }
  }

  /**
   * Reads from an {@link InputStream} to a byte array. This will always 
   * completely fill the byte array, unless there are no more bytes to
   * read from the stream.
   * 
   * @param in the input stream from which to read
   * @param buf the byte array to fill
   * @return the number of bytes read, of <code>-1</code> if at the end of
   * the stream
   *
   * @throws IOException if a stream operation does
   */
  public static int read(InputStream in, byte[] buf) throws IOException {
    int num;
    int off = 0;
    while (off < buf.length &&
            (num = in.read(buf, off, buf.length-off)) != -1) {
      off += num;
    }

    // This will read at least one byte if there are any to be read,
    // so bytes read cannot be zero.
    return off == 0 ? -1 : off;
  } 
}
