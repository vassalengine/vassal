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

/**
 * An exception indicating that an image file is not a recognized type.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class UnrecognizedImageTypeException extends ImageIOException {
  private static final long serialVersionUID = 1L;

  public UnrecognizedImageTypeException() {
    super();
  }

  public UnrecognizedImageTypeException(String filename) {
    super(filename);
  }

  public UnrecognizedImageTypeException(File file) {
    super(file);
  }

  public UnrecognizedImageTypeException(String filename, Throwable cause) {
    super(filename, cause);
  }

  public UnrecognizedImageTypeException(File file, Throwable cause) {
    super(file, cause);
  }
}
