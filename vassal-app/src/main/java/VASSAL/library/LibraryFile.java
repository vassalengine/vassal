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

import java.net.URL;
import java.util.Date;
import com.github.zafarkhaja.semver.Version;

/**
 * One downloadable file, in the release that published it.
 */
public class LibraryFile {
  protected final String   filename;
  protected final URL      url;
  protected final String   sha256;
  protected final Date     date;
  protected final long     size;
  protected final Version  version;
  protected final Version  requires;
  
  public LibraryFile(String   fn,
              URL      u,
              long     n,
              String   sh,
              Date     d,
              Version  v,
              Version  r) {
    filename = fn;
    url      = u;
    size     = n;
    sha256   = sh;
    date     = d;
    version  = v;
    requires = r;
  }

  /**
   * Get the URL to download from
   */
  public URL getURL() {
    return url;
  }

  /**
   * Get the file name
   */
  public String getFileName() {
    return filename;
  }

  /**
   * Get the publication date
   */
  public Date getDate() {
    return date;
  }

  /**
   * Get the release number (redundant?)
   */
  public Version getReleaseNumber() {
    return version;
  }

  /**
   * Get the release number (redundant?)
   */
  public Version getRequired() {
    return requires;
  }

  /**
   * Get the SHA-256 checksum
   */
  public String getChecksum() {
    return sha256;
  }
  
  /**
   * Get size of file in bytes
   */
  public long getSize() {
    return size;
  }
    
  /**
   * Check if this is a module
   */
  public boolean isModule() {
    return filename.endsWith(".vmod"); //NON-NLS
  }

  /**
   * Check if this is a module extension
   */
  public boolean isExtension() {
    return filename.endsWith(".vmdx") || filename.endsWith(".vext"); //NON-NLS
  }
    
  /**
   * Check if this is a save file
   */
  public boolean isSave() {
    return filename.endsWith(".vsav"); //NON-NLS
  }

  /**
   * Check if this is a log file
   */
  public boolean isLog() {
    return filename.endsWith(".vlog"); //NON-NLS
  }
    
  @Override
  public String toString() {
    return "    " + filename + " (" + version + ") @ " + url;
  }
}
//
// EOF
//
