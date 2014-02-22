/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman
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

import static VASSAL.tools.IterableEnumeration.iterate;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.zip.CRC32;
import java.util.zip.CheckedOutputStream;
import java.util.zip.Checksum;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;

import VASSAL.Info;
import VASSAL.tools.concurrent.CountingReadWriteLock;

/**
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class ZipArchive implements FileArchive {
  private final File archiveFile;
  private ZipFile zipFile;

  private boolean modified = false;
  private boolean closed = true;

  private static class Entry {
    public ZipEntry ze;
    public File file;

    public Entry(ZipEntry ze, File file) {
      this.ze = ze;
      this.file = file;
    }

    @Override
    public String toString() {
      return getClass().getName() + "[file=\"" + file + "\", ze=\"" + ze + "\"]";
    }
  }

  private final Map<String,Entry> entries = new HashMap<String,Entry>();

  private final ReadWriteLock rwl = new CountingReadWriteLock();
  private final Lock r = rwl.readLock();
  private final Lock w = rwl.writeLock();

  /**
   * Opens a ZIP archive.
   *
   * @param path the name of the archive
   * @throws IOException
   */
  public ZipArchive(String path) throws IOException {
    this(path, false);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param file the name of the archive
   * @throws IOException
   */
  public ZipArchive(File file) throws IOException {
    this(file, false);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param path the name of the archive
   * @param truncate if <code>true</code>, truncate the archive file on open
   * @throws IOException
   */
  public ZipArchive(String path, boolean truncate) throws IOException {
    this(new File(path), truncate);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param file the name of the archive
   * @param truncate if <code>true</code>, truncate the archive file on open
   * @throws IOException
   */
  public ZipArchive(File file, boolean truncate) throws IOException {
    if (file == null) throw new IllegalArgumentException();
    this.archiveFile = file;

    if (truncate) {
      archiveFile.delete();
    }
  }

  /**
   * Copies a ZIP archive.
   *
   * @param src the name of the source archive
   * @param dst the name of the destination archive
   * @throws IOException
   */
  public ZipArchive(FileArchive src, String dst) throws IOException {
    this(src, new File(dst));
  }

  /**
   * Copies a ZIP archive.
   *
   * @param src the name of the source archive
   * @param dst the name of the destination archive
   * @throws IOException
   */
  public ZipArchive(FileArchive src, File dst) throws IOException {
    this(dst, true);

    final byte[] buf = new byte[8192];

    // copy each entry to the new archive
    for (String name : src.getFiles()) {
      InputStream in = null;
      try {
        in = src.getInputStream(name);

        OutputStream out = null;
        try {
          out = getOutputStream(name);
          IOUtils.copy(in, out, buf);
          out.close();
        }
        finally {
          IOUtils.closeQuietly(out);
        }

        in.close();
      }
      finally {
        IOUtils.closeQuietly(in);
      }
    }

    flush();
  }

  /** {@inheritDoc} */
  public String getName() {
    return archiveFile.getPath();
  }

  /** {@inheritDoc} */
  public File getFile() {
    return archiveFile;
  }

  /** {@inheritDoc} */
  public boolean isClosed() {
    return closed;
  }

  /** {@inheritDoc} */
  public boolean isModified() {
    return modified;
  }

  /**
   * {@inheritDoc}
   *
   * <b>Note:</b> It is impeative the that calling code ensures that this
   * stream is eventually closed, since the returned stream holds a read
   * lock on the archive.
   */
  public InputStream getInputStream(String path) throws IOException {
    r.lock();
    try {
      openIfClosed();

      final Entry e = entries.get(path);
      if (e == null) {
        throw new FileNotFoundException(path + " not in archive");
      }

      InputStream in = null;
      if (e.file != null) {
        in = new FileInputStream(e.file);
      }
      else if (zipFile != null) {
        // NB: Undocumented, but ZipFile.getInputStream can return null!
        in = zipFile.getInputStream(e.ze);
      }

      if (in == null) {
        throw new FileNotFoundException(path + " not in archive");
      }

      return new ZipArchiveInputStream(in);
    }
    catch (IOException ex) {
      r.unlock();
      throw ex;
    }
  }

  /**
   * {@inheritDoc}
   *
   * <b>Note:</b> It is imperative the that calling code ensures that this
   * stream is eventually closed, since the returned stream holds a write
   * lock on the archive.
   */
  public OutputStream getOutputStream(String path) throws IOException {
    return getOutputStream(path, true);
  }

  /**
   * Gets an {@link OutputStream} to write to the given file.
   *
   * <b>Note:</b> It is imperative the that calling code ensures that this
   * stream is eventually closed, since the returned stream holds a write
   * lock on the archive.
   *
   * @param path the path to the file in the archive
   * @param compress whether to compress the file
   * @return an <code>OutputStream</code> for the requested file
   * @throws IOException
   */
  public OutputStream getOutputStream(String path, boolean compress)
                                                           throws IOException {
    w.lock();
    try {
      openIfClosed();

      modified = true;

      // set up new ZipEntry
      final ZipEntry ze = new ZipEntry(path);
      ze.setMethod(compress ? ZipEntry.DEFLATED : ZipEntry.STORED);

      // create new temp file
      final File tf = File.createTempFile("zip", ".tmp", Info.getTempDir());

      // set up new Entry
      final Entry e = new Entry(ze, tf);
      final Entry old = entries.put(path, e);

      // clean up old temp file
      if (old != null && old.file != null) {
        old.file.delete();
      }

      return new ZipArchiveOutputStream(
        new FileOutputStream(e.file), new CRC32(), e.ze
      );
    }
    catch (IOException ex) {
      w.unlock();
      throw ex;
    }
  }

  /** {@inheritDoc} */
  public void add(String path, String extPath) throws IOException {
    add(path, new File(extPath));
  }

  /** {@inheritDoc} */
  public void add(String path, File extPath) throws IOException {
    FileInputStream in = null;
    try {
      in = new FileInputStream(extPath);
      add(path, in);
      in.close();
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  /** {@inheritDoc} */
  public void add(String path, byte[] bytes) throws IOException {
    add(path, new ByteArrayInputStream(bytes));
  }

  /** {@inheritDoc} */
  public void add(String path, InputStream in) throws IOException {
    OutputStream out = null;
    try {
      out = getOutputStream(path);
      IOUtils.copy(in, out);
      out.close();
    }
    finally {
      IOUtils.closeQuietly(out);
    }
  }

  /** {@inheritDoc} */
  public boolean remove(String path) throws IOException {
    w.lock();
    try {
      openIfClosed();

      final Entry e = entries.remove(path);
      if (e != null) {
        modified = true;

        if (e.file != null) {
          e.file.delete();
        }
      }

      return e != null;
    }
    finally {
      w.unlock();
    }
  }

  /** {@inheritDoc} */
  public void revert() throws IOException {
    w.lock();
    try {
      if (!modified) {
        return;
      }

      // delete all temporary files
      for (Entry e : entries.values()) {
        if (e != null && e.file != null) {
          e.file.delete();
        }
      }

      modified = false;
    }
    finally {
      w.unlock();
    }
  }

  /** {@inheritDoc} */
  public void flush() throws IOException {
    w.lock();
    try {
      if (modified) {
        writeToDisk();
      }
    }
    finally {
      w.unlock();
    }
  }

  /** {@inheritDoc} */
  public void close() throws IOException {
    w.lock();
    try {
      if (closed) {
        return;
      }
      else if (modified) {
        writeToDisk();
      }
      else if (zipFile != null) {
        zipFile.close();
        zipFile = null;

        closed = true;
        entries.clear();
      }
    }
    finally {
      w.unlock();
    }
  }

  private void writeToDisk() throws IOException {
    // write all files to a temporary zip archive
    final File tmpFile =
      File.createTempFile("tmp", ".zip", archiveFile.getParentFile());

    ZipOutputStream out = null;
    try {
      out = new ZipOutputStream(
              new BufferedOutputStream(
                new FileOutputStream(tmpFile)));
      out.setLevel(9);

      final byte[] buf = new byte[8192];

      if (zipFile != null) {
        zipFile.close();
        zipFile = null;

        // copy unmodified file into the temp archive
        ZipInputStream in = null;
        try {
          in = new ZipInputStream(
                 new BufferedInputStream(
                   new FileInputStream(archiveFile)));

          ZipEntry ze = null;
          while ((ze = in.getNextEntry()) != null) {
            // skip modified or removed entries
            final Entry e = entries.get(ze.getName());
            if (e == null || e.file != null) continue;

            // We can't reuse entries for compressed files because there's
            // no way to reset all fields to acceptable values.
            if (ze.getMethod() == ZipEntry.DEFLATED) {
              ze = new ZipEntry(ze.getName());
              ze.setTime(ze.getTime());
            }

            out.putNextEntry(ze);
            IOUtils.copy(in, out, buf);

            entries.remove(ze.getName());
          }

          in.close();
        }
        finally {
          IOUtils.closeQuietly(in);
        }
      }

      for (Entry e : entries.values()) {
        // skip removed or unmodified files
        if (e == null || e.file == null) continue;

        // write new or modified file into the temp archive
        FileInputStream in = null;
        try {
          in = new FileInputStream(e.file);
          out.putNextEntry(e.ze);
          IOUtils.copy(in, out, buf);
          in.close();
        }
        finally {
          IOUtils.closeQuietly(in);
        }
      }

      out.close();
    }
    finally {
      IOUtils.closeQuietly(out);
    }

    // Replace old archive with temp archive.
    if (!tmpFile.renameTo(archiveFile)) {
      try {
        FileUtils.forceDelete(archiveFile);
        FileUtils.moveFile(tmpFile, archiveFile);
      }
      catch (IOException e) {
        String err =
          "Unable to overwrite " + archiveFile.getAbsolutePath() + ": ";

        if (!archiveFile.exists()) {
          err += " file does not exist.";
        }
        else if (!archiveFile.canWrite()) {
          err += " file is not writable.";
        }
        else if (!archiveFile.isFile()) {
          err += " not a normal file.";
        }

        err += " Data written to " + tmpFile.getAbsolutePath() + " instead.";
        throw (IOException) new IOException(err).initCause(e);
      }
    }

    // Delete all temporary files
    for (Entry e : entries.values()) {
      if (e != null && e.file != null) {
        e.file.delete();
      }
    }

    closed = true;
    modified = false;
    entries.clear();
  }

  /** {@inheritDoc} */
  public boolean contains(String path) throws IOException {
    r.lock();
    try {
      openIfClosed();
      return entries.containsKey(path);
    }
    finally {
      r.unlock();
    }
  }

  /** {@inheritDoc} */
  public long getSize(String path) throws IOException {
    r.lock();
    try {
      openIfClosed();

      final Entry e = entries.get(path);
      if (e == null) {
        throw new FileNotFoundException(path + " not in archive");
      }

      return e.file == null ? e.ze.getSize() : e.file.length();
    }
    finally {
      r.unlock();
    }
  }

  /** {@inheritDoc} */
  public long getMTime(String path) throws IOException {
    r.lock();
    try {
      openIfClosed();

      final Entry e = entries.get(path);
      if (e == null) {
        throw new FileNotFoundException(path + " not in archive");
      }

      return e.file == null ? e.ze.getTime() : e.file.lastModified();
    }
    finally {
      r.unlock();
    }
  }

  /** {@inheritDoc} */
  public List<String> getFiles() throws IOException {
    r.lock();
    try {
      openIfClosed();
      return new ArrayList<String>(entries.keySet());
    }
    finally {
      r.unlock();
    }
  }

  /** {@inheritDoc} */
  public List<String> getFiles(String root) throws IOException {
    if (root.length() == 0) {
      return getFiles();
    }

    r.lock();
    try {
      openIfClosed();

// FIXME: directories need not have entries in the ZipFile!
//      if (!entries.containsKey(root))
//        throw new FileNotFoundException(root + " not in archive");

      root += '/';
      final ArrayList<String> names = new ArrayList<String>();

      for (String n : entries.keySet()) {
        if (n.startsWith(root)) {
          names.add(n);
        }
      }

      return names;
    }
    finally {
      r.unlock();
    }
  }

  /** Rebuilds the {@link ZipEntries} from our underlying {@link ZipFile}. */
  private synchronized void readEntries() throws IOException {
    entries.clear();

    if (archiveFile.exists() && archiveFile.length() > 0) {
      zipFile = new ZipFile(archiveFile);
      for (ZipEntry e : iterate(zipFile.entries())) {
        entries.put(e.getName(), new Entry(e, null));
      }
    }
  }

  /** Opens the archive if it is closed. */
  private synchronized void openIfClosed() throws IOException {
    if (closed) {
      readEntries();
      modified = false;
      closed = false;
    }
  }

  /** An {@link InputStream} which releases the read lock on close. */
  private class ZipArchiveInputStream extends FilterInputStream {
    public ZipArchiveInputStream(InputStream in) {
      super(in);

      if (in == null) {
        throw new NullPointerException("in == null");
      }
    }

    private boolean closed = false;

    @Override
    public void close() throws IOException {
      if (closed) {
        return;
      }

      try {
        super.close();
      }
      finally {
        r.unlock();
        closed = true;
      }
    }
  }

  /**
   * An {@link OutputStream} which calculates a checksum, counts bytes
   * written, and releases the write lock on close.
   */
  private class ZipArchiveOutputStream extends CheckedOutputStream {
    private ZipEntry entry;
    private long count = 0;

    public ZipArchiveOutputStream(OutputStream out,
                                  Checksum cksum, ZipEntry e) {
      super(out, cksum);

      if (out == null) {
        throw new NullPointerException("out == null");
      }

      if (cksum == null) {
        throw new NullPointerException("cksum == null");
      }

      if (e == null) {
        throw new NullPointerException("e == null");
      }

      entry = e;
    }

    @Override
    public void write(byte[] bytes, int off, int len) throws IOException {
      super.write(bytes, off, len);
      count += len;
    }

    @Override
    public void write(int b) throws IOException {
      super.write(b);
      ++count;
    }

    @Override
    public void flush() throws IOException {
      super.flush();
      entry.setSize(count);
      entry.setCrc(getChecksum().getValue());
    }

    private boolean closed = false;

    @Override
    public void close() throws IOException {
      if (closed) {
        return;
      }

      try {
        super.close();
      }
      finally {
        w.unlock();
        closed = true;
      }
    }
  }

  public static void main(String[] args) throws IOException {
    final ZipArchive archive = new ZipArchive("test.zip");

    // write test
    archive.add("NOTES", "NOTES");
    archive.add("README.txt", "README.txt");

    archive.flush();

    // read test

    InputStream in = null;
    try {
      in = archive.getInputStream("NOTES");
      IOUtils.copy(in, System.out);
      in.close();
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    archive.close();
  }
}
