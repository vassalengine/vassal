/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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

package VASSAL.tools.version;

/**
 * An {@link Exception} indicating a malformed VASSAL version string.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @see Version
 * @see VersionTokenizer
 */
public class VersionFormatException extends Exception {
  private static final long serialVersionUID = 1L;

  /** {@inheritDoc} */
  public VersionFormatException() { }

  /** {@inheritDoc} */
  public VersionFormatException(String message) {
    super(message);
  }

  /** {@inheritDoc} */
  public VersionFormatException(String message, Throwable cause) {
    super(message, cause);
  }

  /** {@inheritDoc} */
  public VersionFormatException(Throwable cause) {
    super(cause);
  }
}
