/*
 * Copyright (c) 2021 by Joel Uckelman
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
package VASSAL.tools.image.tilecache;

import java.io.File;

import VASSAL.tools.image.ImageNotFoundException;

/**
 * An exception indicating that a tile file was not found.
 *
 * @author Joel Uckelman
 * @since 3.5.4
 */
public class TileNotFoundException extends ImageNotFoundException {
  private static final long serialVersionUID = 1L;

  public TileNotFoundException(String filename) {
    super(filename);
  }

  public TileNotFoundException(File file) {
    super(file);
  }

  public TileNotFoundException(String filename, Throwable cause) {
    super(filename, cause);
  }

  public TileNotFoundException(File file, Throwable cause) {
    super(file, cause);
  }
}
