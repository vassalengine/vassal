/*
 * $Id$
 *
 * Copyright (c) 2008-2010 by Joel Uckelman
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

package VASSAL.tools.image;

import java.io.File;
import java.io.IOException;

/**
 * An exception indicating some I/O problem while reading an image file.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ImageIOException extends IOException {
  private static final long serialVersionUID = 1L;

  private final File file;

  protected ImageIOException() {
    this.file = null;
  }

  public ImageIOException(String filename) {
    this(new File(filename));
  }

  public ImageIOException(File file) {
    super(file.getPath());
    this.file = file;
  }

  public ImageIOException(String filename, String message) {
    this(new File(filename), message);
  }

  public ImageIOException(File file, String message) {
    super(file.getPath() + ": " + message);
    this.file = file;
  }

  public ImageIOException(String filename, Throwable cause) {
    this(new File(filename), cause);
  }

  public ImageIOException(File file, Throwable cause) {
    this(file);
    initCause(cause);
  }

  public ImageIOException(String filename, String message, Throwable cause) {
    this(new File(filename), message, cause);
  }

  public ImageIOException(File file, String message, Throwable cause) {
    super(file.getPath() + ": " + message);
    this.file = file;
    initCause(cause);
  }

  public File getFile() {
    return file;
  }
}
