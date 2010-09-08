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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.AbstractQueuedSynchronizer;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.zip.CheckedOutputStream;
import java.util.zip.Checksum;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import static VASSAL.tools.IterableEnumeration.iterate;

/**
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class ZipArchive implements FileArchive {
  private final File archiveFile;
  private ZipFile zipFile;

  private boolean modified = false;
  private boolean closed = false;

  private static class Entry {
    public ZipEntry ze;
    public File file;

    public Entry(ZipEntry ze, File file) {
      this.ze = ze;
      this.file = file;
    }
  }

  private static class CountingReadWriteLock implements ReadWriteLock {
    public Lock readLock()  { return r; }
    public Lock writeLock() { return w; }
    
    private final ReadLock r  = new ReadLock();
    private final WriteLock w = new WriteLock();

    private final Sync sync = new Sync();

    private class ReadLock implements Lock {
      public void lock()   { sync.acquireShared(0); }
      public void unlock() { sync.releaseShared(0); }

      public void lockInterruptibly() {
        throw new UnsupportedOperationException();
      }

      public Condition newCondition() {
        throw new UnsupportedOperationException();
      }
  
      public boolean tryLock() {
        throw new UnsupportedOperationException();
      }

      public boolean tryLock(long time, TimeUnit unit) {
        throw new UnsupportedOperationException();
      }
    }

    private class WriteLock implements Lock {
      public void lock()   { sync.acquire(0); }
      public void unlock() { sync.release(0); }

      public void lockInterruptibly() {
        throw new UnsupportedOperationException();
      }

      public Condition newCondition() {
        throw new UnsupportedOperationException();
      }
  
      public boolean tryLock() {
        throw new UnsupportedOperationException();
      }

      public boolean tryLock(long time, TimeUnit unit) {
        throw new UnsupportedOperationException();
      }
    }

    // Read states are positive, the write state is -1.
    // State 0 means that no locks are held. 

    private static class Sync extends AbstractQueuedSynchronizer {
      private static final long serialVersionUID = 1L;

      @Override
      protected boolean tryAcquire(int dummy) {
        return compareAndSetState(0, -1);
      }

      @Override
      protected boolean tryRelease(int dummy) {
        if (getState() != -1) throw new IllegalMonitorStateException();
        return compareAndSetState(-1, 0);
      }

      @Override
      protected int tryAcquireShared(int dummy) {
        final int s = getState();
        return s >= 0 && compareAndSetState(s, s+1) ? 1 : -1;
      }

      @Override
      protected boolean tryReleaseShared(int dummy) {
        final int s = getState();
        if (s < 1) throw new IllegalMonitorStateException();
        return compareAndSetState(s, s-1);
      }
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

    if (truncate) archiveFile.delete();
    else if (archiveFile.exists()) readEntries();
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
      if (e == null) throw new FileNotFoundException(path + " not in archive");
 
      InputStream in = null; 
      if (e.file != null) {
        in = new FileInputStream(e.file);
      }
      else if (zipFile != null) {
        in = zipFile.getInputStream(e.ze);
      }
      else {
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

      // update the entries map
      Entry e = entries.get(path);
      if (e == null) {
        e = new Entry(null, null);
        entries.put(path, e);
      }

      // set up new ZipEntry
      final ZipEntry ze = new ZipEntry(path);
      ze.setMethod(compress ? ZipEntry.DEFLATED : ZipEntry.STORED);
      e.ze = ze;
    
      // clean up old temp file
      if (e.file != null) e.file.delete();

      // create new temp file
      e.file = TempFileManager.getInstance().createTempFile("zip", ".tmp");

      return new ZipArchiveOutputStream(
        new FileOutputStream(e.file), new CRC32(), e.ze);
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
      if (e != null) modified = true;

      if (e.file != null) e.file.delete();
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
      if (!modified) return;

      // delete all temporary files
      for (String name : entries.keySet()) {
        final Entry e = entries.get(name);
        if (e.file != null) e.file.delete();
      }

      readEntries();
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
      writeToDisk();
    }
    finally {
      w.unlock();
    }
  }
  
  private void writeToDisk() throws IOException {
    if (!modified) return;

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

      for (String name : entries.keySet()) {
        final Entry e = entries.get(name);

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
    try {
      FileUtils.move(tmpFile, archiveFile);
    }
    catch (IOException e) {
      throw (IOException) new IOException(
        "Unable to overwrite " + archiveFile.getAbsolutePath() +
        ", data written to " + tmpFile.getAbsolutePath() + " instead."
      ).initCause(e);
    }

    readEntries();
    modified = false;
  }

  /** {@inheritDoc} */
  public void close() throws IOException {
    w.lock();
    try {
      if (!closed) {
        writeToDisk();

        closed = true;
        modified = false;
        zipFile = null;
        entries.clear();
      }
    }
    finally {
      w.unlock();
    }
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
      if (e == null) throw new FileNotFoundException(path + " not in archive");
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
      if (e == null) throw new FileNotFoundException(path + " not in archive");
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

      final ArrayList<String> names = new ArrayList<String>();
      for (String n : entries.keySet()) {
        names.add(n);
      }
      return names;
    }
    finally {
      r.unlock();
    }
  }

  /** {@inheritDoc} */
  public List<String> getFiles(String root) throws IOException {
    if (root.length() == 0) return getFiles();

    r.lock();
    try {
      openIfClosed();

// FIXME: directories need not have entries in the ZipFile!
//      if (!entries.containsKey(root))
//        throw new FileNotFoundException(root + " not in archive");

      root += File.separator;
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
    zipFile = new ZipFile(archiveFile);

    for (ZipEntry e : iterate(zipFile.entries())) {
      entries.put(e.getName(), new Entry(e, null));
//System.out.println(e.getName());
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
    }

    private boolean closed = false;

    @Override
    public void close() throws IOException {
      if (closed) return;

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
      if (closed) return;
    
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
