/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Joel Uckelman
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

import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

/**
 * @author Joel Uckelman
 * @since 3.2.0
 */
public interface FileArchive extends Closeable {
  /**
   * Gets the path to the archive file.
   *
   * @return the path as a <code>String</code>
   */
  String getName();

  /**
   * Gets the path to the archive file.
   *
   * @return the path as a <code>File</code>
   */
  File getFile();

  /**
   * Gets an {@link InputStream} to read from the given file.
   *
   * @param path the path to the file in the archive
   * @return an <code>InputStream</code> containing the requested file
   * @throws IOException oops
   */
  InputStream getInputStream(String path) throws IOException;

  /**
   * Gets an {@link OutputStream} to write to the given file.
   *
   * @param path the path to the file in the archive
   * @return an <code>OutputStream</code> for the requested file
   * @throws IOException oops
   */
  OutputStream getOutputStream(String path) throws IOException;

  /**
   * Adds a file to the archive.
   *
   * @param path the internal path of the file to be added
   * @param extPath the external path of the file to be added
   * @throws IOException oops
   */
  void add(String path, String extPath) throws IOException;

  /**
   * Adds a file to the archive.
   *
   * @param path the internal path of the file to be added
   * @param extPath the external path to the file to be added
   * @throws IOException oops
   */
  void add(String path, File extPath) throws IOException;

  /**
   * Adds the contents of a byte array to the archive.
   *
   * @param path the internal path of the file to be added
   * @param bytes the bytes to be added
   * @throws IOException oops
   */
  void add(String path, byte[] bytes) throws IOException;

  /**
   * Adds the contents of an {@link InputStream} to the archive.
   *
   * @param path the internal path of the file to be added
   * @param in the <code>InputStream</code> to read from
   * @throws IOException oops
   */
  void add(String path, InputStream in) throws IOException;

  /**
   * Removes a file from the archive.
   *
   * @param path the path to the file in the archive
   * @return <code>true</code> if the file existed in the archive
   * @throws IOException oops
   */
  boolean remove(String path) throws IOException;

  /**
   * Reverts the archive to its last saved state.
   *
   * @throws IOException oops
   */
  void revert() throws IOException;

  /**
   * Forces all changes to the archive to disk.
   *
   * @throws IOException oops
   */
  void flush() throws IOException;

  /**
   * Closes the archive, writing all changes to disk.
   *
   * @throws IOException oops
   */
  @Override
  void close() throws IOException;

  /**
   * Queries whether a file exists in the archive.
   *
   * @param path the path to the file in the archive
   * @return <code>true</code> if the file exists in the archive
   * @throws IOException oops
   */
  boolean contains(String path) throws IOException;

  /**
   * Queries whether the archive is closed.
   *
   * @return <code>true</code> if the archive is closed
   */
  boolean isClosed();

  /**
   * Queries whether the archive has unsaved modifications.
   *
   * @return <code>true</code> if the archive is modified
   */
  boolean isModified();

  /**
   * Gets the size of a file in the archive, in bytes.
   *
   * @param path the path to the file in the archive
   * @return the size of the file, in bytes
   * @throws FileNotFoundException if <code>path</code> is not in the archive
   * @throws IOException oops
   */
  long getSize(String path) throws IOException;

  /**
   * Get the compressed size of a file in the archive, in bytes.
   * Rerurn the uncompressed size by default.
   *
   * @return the compressed size of the file, in bytes
   * @throws FileNotFoundException if <code>path</code> is not in the archive
   * @throws IOException oops
   */
  default long getCompressedSize(String path) throws IOException  {
    return getSize(path);
  }

  /**
   * Gets the modification time of a file in the archive, in milliseconds
   * since the epoch.
   *
   * @param path the path to the file in the archive
   * @return the mtime of the file
   * @throws FileNotFoundException if <code>path</code> is not in the archive
   * @throws IOException oops
   */
  long getMTime(String path) throws IOException;

  /**
   * Gets the list of files in the archive.
   *
   * @return the list of files in the archive
   * @throws IOException oops
   */
  List<String> getFiles() throws IOException;

  /**
   * Gets the list of files under a given directory of the archive.
   *
   * @param root the directory
   * @return the list of files under the given directory
   * @throws FileNotFoundException if <code>root</code> is not in the archive
   * @throws IOException oops
   */
  List<String> getFiles(String root) throws IOException;
}
