/*
 *
 * Copyright (c) 2006 by Rodney Kinney
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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.NoSuchFileException;

public class JarArchive extends DataArchive {
  protected String prefix;

  public JarArchive() {
    this(null);
  }

  public JarArchive(String prefix) {
    super();
    this.prefix = prefix;
  }

  @Override
  public URL getURL(String fileName) throws IOException {
    final URL url = getClass().getResource(getAbsolutePath(fileName));
    if (url != null) return url;

    for (final DataArchive ext : extensions) {
      try {
        return ext.getURL(fileName);
      }
      catch (FileNotFoundException | NoSuchFileException e) {
        // not found in this extension, try the next
      }
    }

    throw new FileNotFoundException(
      "'" + fileName + "' not found in " + getName());
  }

  @Override
  public InputStream getInputStream(String fileName) throws IOException {
    final InputStream in =
      getClass().getResourceAsStream(getAbsolutePath(fileName));
    if (in != null) return in;

    for (final DataArchive ext : extensions) {
      try {
        return ext.getInputStream(fileName);
      }
      catch (FileNotFoundException | NoSuchFileException e) {
        // not found in this extension, try the next
      }
    }

    throw new FileNotFoundException(String.format("'%s' not found in %s", fileName, getName())); //NON-NLS
  }

  protected String getAbsolutePath(String file) {
    return (prefix != null ? "/" + prefix : "") + "/" + file;
  }

  @Override
  public String getName() {
    return prefix != null ? prefix : super.getName();
  }

  /** @deprecated Use {@link #getInputStream(String)} instead. */
  @SuppressWarnings("removal")
  @Deprecated(since = "2020-08-06", forRemoval = true)
  @Override
  public InputStream getFileStream(String fileName) throws IOException {
    return getInputStream(fileName);
  }
}
