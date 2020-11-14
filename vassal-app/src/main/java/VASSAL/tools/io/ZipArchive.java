/*
 * Copyright (c) 2009-2020 by Joel Uckelman
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
import java.nio.file.FileSystemException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.zip.CRC32;
import java.util.zip.CheckedOutputStream;
import java.util.zip.Checksum;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.tools.concurrent.CountingReadWriteLock;

/**
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class ZipArchive implements FileArchive {
  private static final Logger logger = LoggerFactory.getLogger(ZipArchive.class);

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
      return getClass().getName() + "[file=\"" + file + "\", ze=\"" + ze + "\"]"; //NON-NLS
    }
  }

  private final Map<String, Entry> entries = new HashMap<>();

  private final ReadWriteLock rwl = new CountingReadWriteLock();
  private final Lock r = rwl.readLock();
  private final Lock w = rwl.writeLock();

  /**
   * Opens a ZIP archive.
   *
   * @param path the name of the archive
   * @throws IOException oops
   */
  public ZipArchive(String path) throws IOException {
    this(path, false);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param file the name of the archive
   * @throws IOException oops
   */
  public ZipArchive(File file) throws IOException {
    this(file, false);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param path the name of the archive
   * @param truncate if <code>true</code>, truncate the archive file on open
   * @throws IOException oops
   */
  public ZipArchive(String path, boolean truncate) throws IOException {
    this(new File(path), truncate);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param file the name of the archive
   * @param truncate if <code>true</code>, truncate the archive file on open
   * @throws IOException oops
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
   * @throws IOException oops
   */
  public ZipArchive(FileArchive src, String dst) throws IOException {
    this(src, new File(dst));
  }

  /**
   * Copies a ZIP archive.
   *
   * @param src the name of the source archive
   * @param dst the name of the destination archive
   * @throws IOException oops
   */
  public ZipArchive(FileArchive src, File dst) throws IOException {
    this(dst, true);

    final byte[] buf = new byte[8192];

    // copy each entry to the new archive
    for (final String name : src.getFiles()) {
      try (InputStream in = src.getInputStream(name);
           OutputStream out = getOutputStream(name)) {
        IOUtils.copyLarge(in, out, buf);
      }
    }

    flush();
  }

  /** {@inheritDoc} */
  @Override
  public String getName() {
    return archiveFile.getPath();
  }

  /** {@inheritDoc} */
  @Override
  public File getFile() {
    return archiveFile;
  }

  /** {@inheritDoc} */
  @Override
  public boolean isClosed() {
    return closed;
  }

  /** {@inheritDoc} */
  @Override
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
  @Override
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
  @Override
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
   * @throws IOException oops
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
      final File tf = makeTempFileFor(path);

      // set up new Entry
      final Entry e = new Entry(ze, tf);
      final Entry old = entries.put(path, e);

      // clean up old temp file
      deleteEntryTempFile(old);

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
  @Override
  public void add(String path, String extPath) throws IOException {
    add(path, new File(extPath));
  }

  /** {@inheritDoc} */
  @Override
  public void add(String path, File extPath) throws IOException {
    try (FileInputStream in = new FileInputStream(extPath)) {
      add(path, in);
    }
  }

  /** {@inheritDoc} */
  @Override
  public void add(String path, byte[] bytes) throws IOException {
    add(path, new ByteArrayInputStream(bytes));
  }

  /** {@inheritDoc} */
  @Override
  public void add(String path, InputStream in) throws IOException {
    try (OutputStream out = getOutputStream(path)) {
      in.transferTo(out);
    }
  }

  /** {@inheritDoc} */
  @Override
  public boolean remove(String path) throws IOException {
    w.lock();
    try {
      openIfClosed();

      final Entry e = entries.remove(path);
      if (e != null) {
        modified = true;
        deleteEntryTempFile(e);
      }

      return e != null;
    }
    finally {
      w.unlock();
    }
  }

  /** {@inheritDoc} */
  @Override
  public void revert() throws IOException {
    w.lock();
    try {
      if (!modified) {
        return;
      }

      deleteEntryTempFiles();
      modified = false;
    }
    finally {
      w.unlock();
    }
  }

  /** {@inheritDoc} */
  @Override
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
  @Override
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

  private String extensionOf(String path) {
    final int dot = path.lastIndexOf('.');
    return dot == -1 ? "" : path.substring(dot);
  }

  private File makeTempFileFor(String path) throws IOException {
    final String base = FilenameUtils.getBaseName(path) + "_";
    final String ext = extensionOf(path);
    return Files.createTempFile(Info.getTempDir().toPath(), base, ext).toFile();
  }

  private OutputStream openNew(Path p) throws IOException {
    return Files.newOutputStream(
      p, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE
    );
  }

  private OutputStream openExisting(Path p) throws IOException {
    return Files.newOutputStream(
      p, StandardOpenOption.WRITE
    );
  }

  private void writeToZip(OutputStream out) throws IOException {
    try (OutputStream bout = new BufferedOutputStream(out);
         ZipOutputStream zout = new ZipOutputStream(bout)) {
      zout.setLevel(9);
      writeOldEntries(zout);
      writeNewEntries(zout);
    }
  }

  private void writeToDisk() throws IOException {
    if (zipFile == null) {
      // No existing zipfile so no old entries to copy;
      // write directly to the destination
      try (OutputStream out = openNew(archiveFile.toPath())) {
        writeToZip(out);
      }
    }
    else {
      // Destination already exists, must copy old entries;
      // write to temp file first, then move to destination
      final File tmpFile = makeTempFileFor(archiveFile.getName());
      try (OutputStream out = openExisting(tmpFile.toPath())) {
        writeToZip(out);
      }

      try {
        // Close so we can overwrite it
        zipFile.close();
        zipFile = null;

        // Replace old archive with temp archive
        Files.move(tmpFile.toPath(), archiveFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
      }
      catch (IOException e) {
        // Delete the failed temp archive
        try {
          Files.delete(tmpFile.toPath());
        }
        catch (IOException de) {
          logger.error("Failed to delete temp archive " + tmpFile, de);
        }

        // Reopen the original archive
        zipFile = new ZipFile(archiveFile);

        throw e;
      }
    }

    // Success if we're here, so clean up
    writeCleanup();
  }

  private void writeNewEntries(ZipOutputStream zout) throws IOException {
    for (Entry e : entries.values()) {
      // skip removed or unmodified files
      if (e == null || e.file == null) continue;

      // write new or modified file into the archive
      e.ze.setTime(e.file.lastModified());
      zout.putNextEntry(e.ze);
      Files.copy(e.file.toPath(), zout);
    }
  }

  private void writeOldEntries(ZipOutputStream zout) throws IOException {
    if (!archiveFile.exists()) {
      return;
    }

    // copy unmodified entries into the archive
    try (InputStream fin = Files.newInputStream(archiveFile.toPath());
         InputStream bin = new BufferedInputStream(fin);
         ZipInputStream in = new ZipInputStream(bin)) {
      final byte[] buf = new byte[8192];

      ZipEntry ze = null;
      while ((ze = in.getNextEntry()) != null) {
        // skip modified or removed entries
        final Entry e = entries.get(ze.getName());
        if (e == null || e.file != null) continue;

        // We can't reuse entries for compressed files because there's
        // no way to reset all fields to acceptable values.
        if (ze.getMethod() == ZipEntry.DEFLATED) {
          final ZipEntry nze = new ZipEntry(ze.getName());
          nze.setTime(ze.getTime());
          ze = nze;
        }

        zout.putNextEntry(ze);
        IOUtils.copyLarge(in, zout, buf);
      }
    }
  }

  private void deleteEntryTempFile(Entry e) {
    if (e != null && e.file != null) {
      e.file.delete();
    }
  }

  private void deleteEntryTempFiles() {
    // Delete all temporary files for new entries
    entries.values().forEach(e -> deleteEntryTempFile(e));
  }

  private void writeCleanup() {
    deleteEntryTempFiles();
    entries.clear();
    closed = true;
    modified = false;
  }

  /** {@inheritDoc} */
  @Override
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
  @Override
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
  @Override
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
  @Override
  public List<String> getFiles() throws IOException {
    r.lock();
    try {
      openIfClosed();
      return new ArrayList<>(entries.keySet());
    }
    finally {
      r.unlock();
    }
  }

  /** {@inheritDoc} */
  @Override
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
      final ArrayList<String> names = new ArrayList<>();

      for (final String n : entries.keySet()) {
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

  /** Rebuilds the {@link Entry}s from our underlying {@link ZipFile}. */
  private synchronized void readEntries() throws IOException {
    entries.clear();

    if (archiveFile.exists() && archiveFile.length() > 0) {
      zipFile = new ZipFile(archiveFile);
      zipFile.stream().forEach(e -> entries.put(e.getName(), new Entry(e, null)));
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
      super(Objects.requireNonNull(in));
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
    private final ZipEntry entry;
    private long count = 0;

    public ZipArchiveOutputStream(OutputStream out,
                                  Checksum cksum, ZipEntry e) {
      super(Objects.requireNonNull(out), Objects.requireNonNull(cksum));
      entry = Objects.requireNonNull(e);
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
}
