/*
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.library;

import java.util.List;
import java.util.ArrayList;
import com.github.zafarkhaja.semver.Version;

/**
 * A release.  A release contains a number of files.
 */
public class Release {
  protected final Version number;
  protected final List<LibraryFile> files = new ArrayList<>();

  public Release(Version num) {
    number = num;
  }

  /**
   * Get the release number
   */
  public Version getNumber() {
    return number;
  }
    
  /**
   * Get list of contained files
   */
  public List<LibraryFile> getFiles() {
    return files;
  }

  /**
   * Add a file
   */
  public void addFile(LibraryFile file) {
    files.add(file);
  }

  @Override
  public String toString() {
    String ret = "   Release: " + getNumber();
    for (final LibraryFile file : getFiles()) 
      ret += "\n" + file.toString();

    return ret;
  }
}
//
// EOF
//
