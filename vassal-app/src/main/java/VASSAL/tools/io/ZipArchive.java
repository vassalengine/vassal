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
import java.io.FileNotFoundException;
import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
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

import VASSAL.Info;
import VASSAL.tools.concurrent.CountingReadWriteLock;

/**
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class ZipArchive implements FileArchive {
  private final Path archive;
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
    this(Path.of(path), truncate);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param file the name of the archive
   * @param truncate if <code>true</code>, truncate the archive file on open
   * @throws IOException oops
   */
  public ZipArchive(File file, boolean truncate) throws IOException {
    this(file.toPath(), truncate);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param path the name of the archive
   * @param truncate if <code>true</code>, truncate the archive file on open
   * @throws IOException oops
   */
  public ZipArchive(Path path, boolean truncate) throws IOException {
    this.archive = Objects.requireNonNull(path);

    if (truncate) {
      Files.deleteIfExists(archive);
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
    this(src, Path.of(dst));
  }

  /**
   * Copies a ZIP archive.
   *
   * @param src the name of the source archive
   * @param dst the name of the destination archive
   * @throws IOException oops
   */
  public ZipArchive(FileArchive src, File dst) throws IOException {
    this(src, dst.toPath());
  }

  /**
   * Copies a ZIP archive.
   *
   * @param src the name of the source archive
   * @param dst the name of the destination archive
   * @throws IOException oops
   */
  public ZipArchive(FileArchive src, Path dst) throws IOException {
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
    return archive.toString();
  }

  /** {@inheritDoc} */
  @Override
  public File getFile() {
    return archive.toFile();
  }

  public Path getPath() {
    return archive;
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
        in = Files.newInputStream(e.file.toPath());
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
        Files.newOutputStream(e.file.toPath()), new CRC32(), e.ze
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
    try (InputStream in = Files.newInputStream(extPath.toPath())) {
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

  private static String extensionOf(String path) {
    final int dot = path.lastIndexOf('.');
    return dot == -1 ? "" : path.substring(dot);
  }

  private static boolean illegalFilenameChar(int c) {
    // These characters are illegal in filenames on at least one of
    // Windows, Mac, or Linux, or are our escape character '%').
    return c < 0x20 || c == '"' || c == '%' || c == '*' || c == ':'
                    || c == '<' || c == '>' || c == '?' || c == '|';
  }

  private static String escapeFilenameChar(int c) {
    return String.format("%%%1$02X", c);
  }

  private static String sanitize(String filename) {
    // Don't write out temp filenames which contain illegal characters.
    final int len = filename.length();
    final StringBuilder b = new StringBuilder(len);
    for (int i = 0; i < len; ++i) {
      final char c = filename.charAt(i);
      b.append(illegalFilenameChar(c) ? escapeFilenameChar(c) : c);
    }
    return b.toString();
  }

  private static File makeTempFileFor(String path) throws IOException {
    return makeTempFileFor(path, Info.getTempDir().toPath());
  }

  private static File makeTempFileFor(String path, Path tmpDir) throws IOException {
    final String base = sanitize(FilenameUtils.getBaseName(path)) + "_";
    final String ext = extensionOf(path);
    Files.createDirectories(tmpDir);
    return Files.createTempFile(tmpDir, base, ext).toFile();
  }

  private static OutputStream openNew(Path p) throws IOException {
    return Files.newOutputStream(
      p, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE
    );
  }

  private void writeToZip(Path oldArchive, OutputStream out) throws IOException {
    try (OutputStream bout = new BufferedOutputStream(out);
         ZipOutputStream zout = new ZipOutputStream(bout)) {
      zout.setLevel(9);
      if (oldArchive != null) {
        writeOldEntries(oldArchive, zout);
      }
      writeNewEntries(zout);
    }
  }

  private void writeToDisk() throws IOException {
    final boolean zipWasOpen = zipFile != null;
    if (zipWasOpen) {
      // Close so we can move the original file
      zipFile.close();
      zipFile = null;
    }

    Path bak = null;
    try {
      // Move the old file out of the way, if it exists
      if (Files.exists(archive)) {
        bak = archive.resolveSibling(archive.getFileName().toString() + ".bak");
        Files.move(archive, bak);
      }

      // Write the file
      try (OutputStream out = openNew(archive)) {
        writeToZip(bak, out);
      }
      catch (final IOException e) {
        // Something went wrong whilst writing
        if (bak == null) {
          // No original, delete bad file
          Files.deleteIfExists(archive);
        }
        else {
          // Reinstate original file
          Files.move(bak, archive, StandardCopyOption.REPLACE_EXISTING);
        }

        throw e;
      }
    }
    catch (final IOException e) {
      if (zipWasOpen) {
        // Reopen the original archive
        zipFile = new ZipFile(archive.toFile());
      }
      throw e;
    }

    // Success if we're here, so clean up
    writeCleanup();

    if (bak != null) {
      Files.deleteIfExists(bak);
    }
  }

  private void writeNewEntries(ZipOutputStream zout) throws IOException {
    for (final Entry e : entries.values()) {
      // skip removed or unmodified files
      if (e == null || e.file == null) continue;

      // write new or modified file into the archive
      e.ze.setTime(e.file.lastModified());
      zout.putNextEntry(e.ze);
      Files.copy(e.file.toPath(), zout);
    }
  }

  private void writeOldEntries(Path oldArchive, ZipOutputStream zout) throws IOException {
    // copy unmodified entries into the archive
    try (InputStream fin = Files.newInputStream(oldArchive);
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

  private void writeCleanup() {
    deleteEntryTempFiles();
    entries.clear();
    closed = true;
    modified = false;
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
  public long getCompressedSize(String path) throws IOException {
    r.lock();
    try {
      openIfClosed();

      final Entry e = entries.get(path);
      if (e == null) {
        throw new FileNotFoundException(path + " not in archive");
      }

      return e.file == null ? e.ze.getCompressedSize() : e.file.length();
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

    if (Files.exists(archive) && Files.size(archive) > 0) {
      zipFile = new ZipFile(archive.toFile());
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
