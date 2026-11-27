/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.launch;

import java.io.File;
import java.io.IOException;

/**
 * Thrown when the directories of the image tile cache cannot be created.
 * This is a problem with the user's disk, permissions or anti-virus program,
 * not a bug, and is reported as such.
 */
public class TileCacheException extends IOException {
  private static final long serialVersionUID = 1L;

  private final File dir;

  public TileCacheException(File dir, IOException cause) {
    super(cause.getMessage(), cause);
    this.dir = dir;
  }

  public File getDirectory() {
    return dir;
  }
}
