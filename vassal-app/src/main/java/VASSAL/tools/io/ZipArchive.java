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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.FilterOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.FileSystem;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.spi.FileSystemProvider;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import VASSAL.Info;
import VASSAL.tools.concurrent.CountingReadWriteLock;

/**
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class ZipArchive implements FileArchive {
  private final Path archiveFile;
  private FileSystem zipfs = null;

  private static FileSystemProvider getZipFSProvider() throws IOException {
    final FileSystemProvider prov =
      FileSystemProvider.installedProviders()
                        .stream()
                        .filter(p -> "jar".equals(p.getScheme()))
                        .findFirst()
                        .orElse(null);
    if (prov == null) {
      throw new IOException("failed to find ZIP FileSystemProvider");
    }

    return prov;
  }

  private static FileSystem getFileSystem(Path path, boolean truncate) throws IOException {
    Map<String, Object> env;

    if (truncate) {
      Files.deleteIfExists(path);
      env = Map.of("create", "true");
    }
    else {
      env = Collections.emptyMap();
    }

    final FileSystemProvider prov = getZipFSProvider();
    return prov.newFileSystem(path, env);
  }

  private boolean modified = false;
  private boolean closed = true;

  private final Map<String, Path> entries = new HashMap<>();

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
    archiveFile = Objects.requireNonNull(file).toPath();
    if (truncate) {
      // create an empty archive
      zipfs = getFileSystem(archiveFile, true);
      closed = false;
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

    // copy each entry to the new archive
    for (String name : src.getFiles()) {
      try (InputStream in = src.getInputStream(name)) {
        final Path dp = zipfs.getPath(name);
        ensureParentsExist(dp);
        Files.copy(in, dp);
      }
    }

    flush();
  }

  /** {@inheritDoc} */
  @Override
  public String getName() {
    return archiveFile.toString();
  }

  /** {@inheritDoc} */
  @Override
  public File getFile() {
    return archiveFile.toFile();
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

      final Path p = entries.get(path);
      if (p == null) {
        throw new FileNotFoundException(path + " not in archive");
      }

      return new ZipArchiveInputStream(Files.newInputStream(p));
    }
    catch (IOException ex) {
      // the returned stream unlocks when there is no exception
      r.unlock();
      throw ex;
    }
  }

  private void ensureParentsExist(Path p) throws IOException {
    final Path parent = p.getParent();
    if (parent != null) {
      Files.createDirectories(parent);
    }
  }

  private Path makeTempDestination(String path) throws IOException {
    // create new temp file
// FIXME: maybe start using system temp dir?
    final Path tmpdir = Info.getTempDir().toPath();
    final Path p = Files.createTempFile(tmpdir, "zip", ".tmp");

    final Path old = entries.put(path, p);

    // clean up old temp file
    if (old != null && old.getFileSystem() != zipfs) {
      Files.delete(old);
    }

    return p;
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
   * @throws IOException
   */
  public OutputStream getOutputStream(String path, boolean compress)
                                                           throws IOException {
    w.lock();
    try {
      openIfClosed();
      modified = true;
      final Path p = makeTempDestination(path);
      return new ZipArchiveOutputStream(Files.newOutputStream(p));
    }
    catch (IOException ex) {
      // the returned stream unlocks when there is no exception
      w.unlock();
      throw ex;
    }
  }

  /** {@inheritDoc} */
  @Override
  public void add(String path, String extPath) throws IOException {
    add(path, Path.of(extPath));
  }

  /** {@inheritDoc} */
  @Override
  public void add(String path, File extPath) throws IOException {
    add(path, extPath.toPath());
  }

  public void add(String path, Path extPath) throws IOException {
    w.lock();
    try {
      openIfClosed();
      modified = true;
      final Path p = makeTempDestination(path);
      Files.copy(extPath, p, StandardCopyOption.REPLACE_EXISTING);
    }
    finally {
      w.unlock();
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
    w.lock();
    try {
      openIfClosed();
      modified = true;
      final Path p = makeTempDestination(path);
      Files.copy(in, p, StandardCopyOption.REPLACE_EXISTING);
    }
    finally {
      w.unlock();
    }
  }

  private void deleteTempFile(Path p) throws IOException {
    if (p.getFileSystem() != zipfs) {
      Files.delete(p);
    }
  }

  private void deleteTempFiles() throws IOException {
    for (Path p : entries.values()) {
      deleteTempFile(p);
    }
  }

  /** {@inheritDoc} */
  @Override
  public boolean remove(String path) throws IOException {
    w.lock();
    try {
      openIfClosed();

      final Path p = entries.remove(path);
      if (p != null) {
        modified = true;
        deleteTempFile(p);
      }

      return p != null;
    }
    finally {
      w.unlock();
    }
  }

  private void cleanup() throws IOException {
    if (!closed) {
      deleteTempFiles();
      entries.clear();

      if (zipfs != null) {
        zipfs.close();
        zipfs = null;
      }

      modified = false;
      closed = true;
    }
  }

  /** {@inheritDoc} */
  @Override
  public void revert() throws IOException {
    w.lock();
    try {
      if (modified) {
        cleanup();
      }
    }
    finally {
      w.unlock();
    }
  }

  /** {@inheritDoc} */
  @Override
  public void flush() throws IOException {
    close();
  }

  /** {@inheritDoc} */
  @Override
  public void close() throws IOException {
    w.lock();
    try {
      if (!closed) {
        if (modified) {
          writeToDisk();
        }
        cleanup();
      }
    }
    finally {
      w.unlock();
    }
  }

  private void writeToDisk() throws IOException {
    // write all files to a temporary zip archive
    final Path tmpFile = Files.createTempFile(archiveFile.getParent(), "tmp", ".zip");
    try (FileSystem tmpfs = getFileSystem(tmpFile, true)) {
      for (Map.Entry<String, Path> e : entries.entrySet()) {
        final Path dst = tmpfs.getPath(e.getKey());
        ensureParentsExist(dst);
        Files.copy(e.getValue(), dst);
      }
    }

    if (zipfs != null) {
      zipfs.close();
      zipfs = null;
    }

    // replace old archive with temp archive
    try {
      Files.deleteIfExists(archiveFile);
      Files.move(tmpFile, archiveFile);
    }
    catch (IOException e) {
      final String fmt = "Unable to overwrite %s: %s Data written to %s instead.";
      throw new IOException(
        String.format(fmt, archiveFile, e.getMessage(), tmpFile), e
      );
    }
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

      final Path p = entries.get(path);
      if (p == null) {
        throw new FileNotFoundException(path + " not in archive");
      }

      return Files.size(p);
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

      final Path p = entries.get(path);
      if (p == null) {
        throw new FileNotFoundException(path + " not in archive");
      }

      return Files.getLastModifiedTime(p).toMillis();
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
      return entries.keySet()
                    .stream()
                    .filter(n -> n.startsWith(root + '/'))
                    .collect(Collectors.toList());
    }
    finally {
      r.unlock();
    }
  }

  /** Rebuilds the {@link Entry}s from our underlying ZIP archive. */
  private synchronized void readEntries() throws IOException {
    entries.clear();

    if (Files.exists(archiveFile) && Files.size(archiveFile) > 0) {
      zipfs = getFileSystem(archiveFile, false);
      for (Path root : zipfs.getRootDirectories()) {
        try (Stream<Path> s = Files.walk(root)) {
          s.filter(Files::isRegularFile).forEach(
            p -> entries.put(p.getRoot().relativize(p).toString(), p)
          );
        }
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

  /** An {@link OutputStream} which releases the write lock on close. */
  private class ZipArchiveOutputStream extends FilterOutputStream {
    public ZipArchiveOutputStream(OutputStream out) {
      super(Objects.requireNonNull(out));
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
