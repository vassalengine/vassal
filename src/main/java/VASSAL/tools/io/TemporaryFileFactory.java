/*
 * $Id$
 *
 * Copyright (c) 2010 by Joel Uckelman
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
package VASSAL.tools.io;

import java.io.File;
import java.io.IOException;

/**
 * A factory for temporary files.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public interface TemporaryFileFactory {
  /**
   * Creates a temporary file.
   *
   * @return a temporary file
   * @throws IOException if the file cannot be created
   */
  public File create() throws IOException;
}
