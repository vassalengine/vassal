/*
 * $Id: DataArchive.java 3807 2008-07-09 23:13:30Z uckelman $
 *
 * Copyright (c) 2008 by Joel Uckelman
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
package VASSAL.tools;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Some general purpose file manipulation utilities.
 * 
 * @author Joel Uckelman
 * @since 3.1.0
 */ 
public class FileUtils {
  private FileUtils() {}

  /**
   * Deletes a file.
   *
   * @param file the file to delete
   * @throws IOException if the file cannot be deleted
   */    
  public static void delete(File file) throws IOException {
    if (!file.delete())
      throw new IOException("Failed to delete " + file.getAbsolutePath());
  }

  /**
   * Creates all directories named by a path.
   *
   * @param dir the path to create
   * @throws IOException if the path cannot be created
   */ 
  public static void mkdirs(File dir) throws IOException {
    if (!dir.mkdirs()) throw new IOException(
      "Failed to create directory " + dir.getAbsolutePath());
  }

  /**
   * Recursively deletes all files in the tree rooted at the given path.
   *
   * @param base the root to delete
   * @throws IOException if some file cannot be deleted
   */ 
  public static void recursiveDelete(File base) throws IOException {
    // we delete as many files as we can
    final List<File> failed = new ArrayList<File>();
    recursiveDeleteHelper(base, failed);

    // if any deletions failed, we list them
    if (!failed.isEmpty()) {
      final StringBuilder sb = new StringBuilder("Failed to delete");
      for (File f : failed) sb.append(' ').append(f.getAbsolutePath());
      throw new IOException(sb.toString());
    }
  }

  private static void recursiveDeleteHelper(File parent, List<File> failed) {
    // delete children, depth first
    if (parent.isDirectory()) {
      for (File child : parent.listFiles()) {
        recursiveDeleteHelper(child, failed);
      }
    }

    // store leaves which can't be deleted in the failed list
    if (!parent.delete()) failed.add(parent);
  }
}
