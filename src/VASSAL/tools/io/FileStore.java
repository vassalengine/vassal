/*
 * $Id$
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * An interface for providing read access to files.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public interface FileStore extends Closeable {
  /**
   * Queries whether a file exists in the store.
   *
   * @param path the path to the file in the store
   * @return <code>true</code> if the file exists in the store
   *
   * @throws IOException
   */
  public boolean contains(String path) throws IOException;

  /**
   * Gets an {@link InputStream} to read from the given file.
   *
   * @param path the path to the file in the store
   * @return an <code>InputStream</code> containing the requested file
   *
   * @throws IOException
   */
  public InputStream getInputStream(String path) throws IOException;

  /**
   * Gets the size of a file in the store, in bytes.
   *
   * @param path the path to the file in the store
   * @return the size of the file, in bytes
   *
   * @throws FileNotFoundException if <code>path</code> is not in the store
   * @throws IOException
   */
  public long getSize(String path) throws IOException;

  /**
   * Gets the modification time of a file in the store, in milliseconds
   * since the epoch.
   *
   * @param path the path to the file in the store
   * @return the mtime of the file
   *
   * @throws FileNotFoundException if <code>path</code> is not in the store
   * @throws IOException
   */
  public long getMTime(String path) throws IOException;

  /**
   * Gets the list of files in the store.
   *
   * @return the list of files in the store
   *
   * @throws IOException
   */
  public List<String> getFiles() throws IOException;

  /**
   * Gets the list of files under a given directory of the store.
   *
   * @param root the directory
   * @return the list of files under the given directory
   *
   * @throws FileNotFoundException if <code>root</code> is not in the store
   * @throws IOException
   */
  public List<String> getFiles(String root) throws IOException;

  /**
   * Closes the store.
   *
   * @throws IOException
   */
  public void close() throws IOException;

  /**
   * Checks whether the store is closed.
   *
   * @return <code>true</code> if the archive is closed
   */
  public boolean isClosed();
}
