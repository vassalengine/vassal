/*
 * $Id$
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

package VASSAL.tools.image;

import java.io.File;
import java.io.FileNotFoundException;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ImageNotFoundException extends FileNotFoundException {
  private static final long serialVersionUID = 1L;

  private final File file; 

  public ImageNotFoundException(String filename) {
    this(new File(filename));
  }

  public ImageNotFoundException(File file) {
    this.file = file;
  }

  public ImageNotFoundException(String filename, Throwable cause) {
    this(filename);
    initCause(cause);
  }

  public ImageNotFoundException(File file, Throwable cause) {
    this(file);
    initCause(cause);
  }

  public File getFile() {
    return file;
  }
}
