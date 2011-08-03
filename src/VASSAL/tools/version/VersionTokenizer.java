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

import java.util.NoSuchElementException;

/**
 * Converts version numbers into a series of integers.
 * The integers thus returned from two different tokenizers may be
 * compared to determine the temporal ordering of two versions. Invalid
 * version numbers may be parsed up to the point where they become invalid.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @see Version
 * @see VersionFormatException
 */
public interface VersionTokenizer {
  /**
   * Returns <code>true</code> if the version <code>String</code> is
   * not fully parsed.
   *
   * @return <code>true</code> if {@link #next()} will return more
   * integers
   */
  public boolean hasNext();

  /**
   * Returns an integer representing the next token.
   *
   * @return the integer representing the next token
   * @throws VersionFormatException if the string deviates from
   * the current version formatting rules at the next token.
   * @throws NoSuchElementException if this method is called when
   * {@link hasNext()} would return <code>false</code>.
   */
  public int next() throws VersionFormatException;
}
