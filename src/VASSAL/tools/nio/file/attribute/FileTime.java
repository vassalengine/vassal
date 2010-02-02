/*
 * $Id: URLUtils.java 4860 2009-01-15 17:15:23Z uckelman $
 *
 * Copyright (c) 2009 by Joel Uckelman
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

package VASSAL.tools.nio.file.attribute;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.TimeUnit;

import VASSAL.tools.HashCode;

public final class FileTime implements Comparable<FileTime> {
 
  /** The time in milliseconds. */ 
  protected final long ms;

  protected FileTime(long value) {
    ms = value;
  }

  public int compareTo(FileTime other) {
    return this.ms < other.ms ? -1 : this.ms > other.ms ? 1 : 0;
  }

  public static FileTime from(long value, TimeUnit unit) {
    return new FileTime(TimeUnit.MILLISECONDS.convert(value, unit));
  }

  public static FileTime fromMillis(long value) {
    return new FileTime(value);
  }

  public long to(TimeUnit unit) {
    return unit.convert(ms, TimeUnit.MILLISECONDS);
  }

  public long toMillis() {
    return ms;
  }  

  protected static final DateFormat ISO8601 =
    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");

  /** {@inheritDoc} */
  @Override
  public String toString() {
// FIXME: need to check that this is correct for UTC
    return ISO8601.format(new Date(ms));
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return HashCode.hash(ms);
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o.getClass() != this.getClass()) return false;
    return ((FileTime) o).ms == this.ms;
  }
}
