/*
 * $Id$
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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

import java.io.ByteArrayInputStream;
import java.io.BufferedInputStream;
import java.io.Closeable;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.zip.ZipFile;
import javax.imageio.stream.ImageInputStream;

import VASSAL.tools.logging.Logger;

/**
 * General I/O stream manipulation utilities. This class provides static
 * utility methods to reduce boilerplate I/O code.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */ 
public class IOUtils {
  // Portions based on org.apache.commons.io.IOUtils.

  /** The default size for input buffers. */
  public static final int BUFFER_SIZE = 4096;

  /**
   * Copies bytes from an <code>InputStream</code> to an
   * <code>OutputStream</code> via a <code>byte</code> buffer. This
   * method buffers input internally, so the input stream should not
   * be a <code>BufferedInputStream</code>.
   *
   * @param in the source
   * @param out the destination
   * @param buffer the buffer
   * @throws IOException if one occurs while reading or writing.
   */
  public static void copy(InputStream in, OutputStream out, byte[] buffer)
      throws IOException {
    int n = 0;
    while ((n = in.read(buffer)) >= 0) out.write(buffer, 0, n);
  }

  /**
   * Copies bytes from an <code>InputStream</code> to an
   * <code>OutputStream</code>. This method buffers input
   * internally, so the input stream should not be a
   * <code>BufferedInputStream</code>.
   *
   * @param in the source
   * @param out the destination
   * @throws IOException if one occurs while reading or writing.
   */
  public static void copy(InputStream in, OutputStream out)
      throws IOException {
    copy(in, out, new byte[BUFFER_SIZE]);
  }

  /**
   * Copies chars from a <code>Reader</code> to a <code>Writer</code>
   * via a <code>char</code> buffer. This method buffers input internally,
   * so the <code>Reader</code> should not be a <code>BufferedReader</code>.
   *
   * @param in the source 
   * @param out the destination
   * @param buffer the buffer
   * @throws IOException if one occurs while reading or writing.
   */
  public static void copy(Reader in, Writer out, char[] buffer)
      throws IOException {
    int n = 0;
    while ((n = in.read(buffer)) >= 0) out.write(buffer, 0, n);
  }

  /**
   * Copies chars from a <code>Reader</code> to a <code>Writer</code>.
   * This method buffers input internally, so the <code>Reader</code>
   * should not be a <code>BufferedReader</code>.
   *
   * @param in the source 
   * @param out the destination
   * @throws IOException if one occurs while reading or writing.
   */
  public static void copy(Reader in, Writer out) throws IOException {
    copy(in, out, new char[BUFFER_SIZE]);
  }
  
  /**
   * Copies bytes from an <code>InputStream</code> to chars for a
   * <code>Writer</code>, using the default character encoding.
   * This method buffers input internally, so the input stream should
   * not be a <code>BufferedInputStream</code>.
   *
   * @param in the source
   * @param out the destination
   * @throws IOException if one occurs while reading or writing.
   */
  public static void copy(InputStream in, Writer out) throws IOException {
    copy(new InputStreamReader(in), out);
  }

  /**
   * Copies bytes from an <code>InputStream</code> to chars for a
   * <code>Writer</code>, using the specified character encoding.
   * This method buffers input internally, so the input stream should
   * not be a <code>BufferedInputStream</code>.
   *
   * @param in the source
   * @param out the destination
   * @param encoding the character encoding; <code>null</code> for default
   * @throws IOException if one occurs while reading or writing.
   */
  public static void copy(InputStream in, Writer out, String encoding)
      throws IOException {
    if (encoding == null) {
      copy(in, out);
    }
    else {
      copy(new InputStreamReader(in, encoding), out);
    }
  }

  /**
   * Copies chars from a <code>Reader</code> to bytes for an
   * <code>OutputStream</code>, using the default character encoding.
   * This method buffers input internally, so the input stream should
   * not be a <code>BufferedReader</code>.
   *
   * @param in the source
   * @param out the destination
   * @throws IOException if one occurs while reading or writing.
   */
  public static void copy(Reader in, OutputStream out)
      throws IOException {
    final OutputStreamWriter osw = new OutputStreamWriter(out);
    copy(in, osw);
    osw.flush();
  }

 /**
   * Copies chars from a <code>Reader</code> to bytes for an
   * <code>OutputStream</code>, using the specified character encoding.
   * This method buffers input internally, so the input stream should
   * not be a <code>BufferedReader</code>.
   *
   * @param in the source
   * @param out the destination
   * @param encoding the character encoding; <code>null</code> for default
   * @throws IOException if one occurs while reading or writing.
   */
  public static void copy(Reader in, OutputStream out, String encoding)
      throws IOException {
    if (encoding == null) {
      copy(in, out);
    }
    else {
      final OutputStreamWriter osw = new OutputStreamWriter(out, encoding);
      copy(in, osw);
      osw.flush();
    }
  }

  /**
   * Copies a <code>String</code> with the specified encoding to
   * an <code>InputStream</code>.
   *
   * @param input the input <code>String</code>
   * @param encoding the character encoding; <code>null</code> for default
   * @return an input stream
   */
  public static InputStream toInputStream(String input, String encoding)
      throws IOException {
    return new ByteArrayInputStream(
      encoding != null ? input.getBytes(encoding) : input.getBytes());
  }

  /**
   * Copies bytes from an <code>InputStream</code> to a <code>String</code>
   * using the default character encoding. This method buffers input
   * internally, so the input stream should not be a
   * <code>BufferedInputStream</code>.
   *
   * @param in the source
   * @return the source as a string
   * @throws IOException if one occurs while reading.
   */
  public static String toString(InputStream in) throws IOException {
    return toString(in, null);
  }

  /**
   * Copies bytes from an <code>InputStream</code> to a <code>String</code>
   * using the specified character encoding. This method buffers input
   * internally, so the input stream should not be a
   * <code>BufferedInputStream</code>.
   *
   * @param in the source
   * @param encoding the character encoding; <code>null</code> for default
   * @return the source as a string
   * @throws IOException if one occurs while reading.
   */
  public static String toString(InputStream in, String encoding)
      throws IOException {
    final StringWriter out = new StringWriter();
    copy(in, out, encoding);
    return out.toString();
  }

  /**
   * Copies chars from a <code>Reader</code> to a <code>String</code>
   * using the specified character encoding. This method buffers input
   * internally, so the reader should not be a
   * <code>BufferedReader</code>.
   *
   * @param in the source
   * @param encoding the character encoding; <code>null</code> for default
   * @return the source as a string
   * @throws IOException if one occurs while reading.
   */
  public static String toString(Reader in) throws IOException {
    final StringWriter out = new StringWriter();
    copy(in, out);
    return out.toString();
  }

  /**
   * Copies bytes from an <code>InputStream</code> to a <code>byte[]</code>.
   * This method buffers input internally, so the input stream should not be
   * a <code>BufferedInputStream</code>.
   *
   * @param in the source
   * @return the source as a <code>byte[]</code>
   * @throws IOException if one occurs while reading.
   */
  public static byte[] toByteArray(InputStream in) throws IOException {
    // try to size the buffer correctly if the stream provides size information
    final int available = in.available();
    final FastByteArrayOutputStream out = available > 0 ? 
      new FastByteArrayOutputStream(available) :
      new FastByteArrayOutputStream();

    copy(in, out);
    return out.toByteArray();
  }

  /**
   * Close a {@link Closeable} unconditionally. Equivalent to
   * calling <code>c.close()</code> when <code>c</code> is nonnull.
   * {@link IOException}s are quietly logged, as there is generally
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
      Logger.log(e);
    }
  }

  // FIXME: Remove in Java 1.6+, when ServerSocket implements Closeable 
  public static void closeQuietly(ServerSocket s) {
    if (s == null) return;

    try {
      s.close();
    }
    catch (IOException e) {
      Logger.log(e);
    }
  }

  // FIXME: Remove in Java 1.6+, when Socket implements Closeable 
  public static void closeQuietly(Socket s) {
    if (s == null) return;

    try {
      s.close();
    }
    catch (IOException e) {
      Logger.log(e);
    }
  }

  // Why doesn't ZipFile implement Closeable? Argh!
  /**
   * Close a {@link ZipFile} unconditionally. Equivalent to
   * calling <code>z.close()</code> when <code>z</code> is nonnull.
   * {@link IOException}s are quietly logged, as there is generally
   * nothing that can be done about exceptions on closing.
   *
   * @param z a (possibly <code>null</code>) <code>ZipFile</code>
   */
  public static void closeQuietly(ZipFile z) {
    if (z == null) return;

    try {
      z.close();
    }
    catch (IOException e) {
      Logger.log(e);
    }
  }

  // Why doesn't ImageInputStream implement Closeable? Argh!
  /**
   * Close an {@link ImageInputStream} unconditionally. Equivalent to
   * calling <code>s.close()</code> when <code>s</code> is nonnull.
   * {@link IOException}s are quietly logged, as there is generally
   * nothing that can be done about exceptions on closing.
   *
   * @param s a (possibly <code>null</code>) <code>ImageInputStream</code>
   */
  public static void closeQuietly(ImageInputStream s) {
    if (s == null) return;

    try {
      s.close();
    }
    catch (IOException e) {
      // ImageInputStreamImpl.close() rather ridiculously throws an
      // IOException if the stream is already closed. This is always done
      // via ImageInputStreamImpl.checkClosed(). We check the top of the
      // stack trace to see if the exception came from checkClosed(), and
      // only log IOExceptions which did not.
      final StackTraceElement stack[] = e.getStackTrace();
      if (stack.length == 0 ||
          !"checkClosed".equals(stack[0].getMethodName())) {
        Logger.log(e);
      }
    }
  }

  /**
   * Tests whether the contents of two {@link InputStream}s are the same.
   *
   * @param ain one stream
   * @param bin the other stream
   * @return <code>true</code> if the contents of the streams are equal
   * @throws NullPointerException if either input is null
   * @throws IOException if a stream operation does
   */ 
  public static boolean contentEquals(InputStream ain, InputStream bin)
                                                           throws IOException {
    if (ain == null) throw new NullPointerException();
    if (bin == null) throw new NullPointerException();

    if (ain == bin) return true;
    
    final byte[] abuf = new byte[BUFFER_SIZE];
    final byte[] bbuf = new byte[BUFFER_SIZE];

    for (;;) {
      final int anum = fillBuffer(ain, abuf);
      final int bnum = fillBuffer(bin, bbuf);
     
      if (anum != bnum) return false;

      if (anum == -1) return true;

      for (int i = 0; i < anum; ++i) if (abuf[i] != bbuf[i]) return false;
    }
  }

  private static int fillBuffer(InputStream in, byte[] buf)
                                                           throws IOException {
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
