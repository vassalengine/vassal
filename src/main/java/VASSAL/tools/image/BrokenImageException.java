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
 * An exception indicating that an image file is broken in some way.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class BrokenImageException extends ImageIOException {
  private static final long serialVersionUID = 1L;

  protected BrokenImageException() {
    super();
  }

  public BrokenImageException(String filename) {
    super(filename);
  }

  public BrokenImageException(File file) {
    super(file);
  }

  public BrokenImageException(String filename, String message) {
    super(filename, message);
  }

  public BrokenImageException(File file, String message) {
    super(file, message);
  }

  public BrokenImageException(String filename, Throwable cause) {
    super(filename, cause);
  }

  public BrokenImageException(File file, Throwable cause) {
    super(file, cause);
  }

  public BrokenImageException(String filename, String message,
                                               Throwable cause) {
    super(filename, message, cause);
  }

  public BrokenImageException(File file, String message, Throwable cause) {
    super(file, message, cause);
  }
}
