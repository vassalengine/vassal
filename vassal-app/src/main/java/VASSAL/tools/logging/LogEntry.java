/*
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.logging;

import java.io.Serializable;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.TimeZone;

import VASSAL.tools.ThrowableUtils;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
@Deprecated(since = "2021-08-06", forRemoval = true)
public class LogEntry implements Serializable {
  private static final long serialVersionUID = 1L;

  public static final int MESSAGE = 0;
  public static final int WARNING = 1;
  public static final int ERROR = 2;
  public static final int BUG = 3;
  public static final int DEBUG = 4;
  public static final int SYSTEM = 5;

  public final long timestamp;
  public final int type;
  public final String message;
  public final String trace;
  public final boolean wait;

  public LogEntry(int type, Throwable thrown, String message, boolean wait) {
    this(System.currentTimeMillis(), type, thrown, message, wait);
  }

  public LogEntry(long timestamp, int type,
                  Throwable thrown, String message, boolean wait) {

    if (thrown == null && message == null)
      throw new IllegalArgumentException();

    this.timestamp = timestamp;
    this.type = type;
    this.message = message;
    this.trace = thrown != null ? ThrowableUtils.getStackTrace(thrown) : null;
    this.wait = wait;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();

    final DateFormat dateFormat =
      new SimpleDateFormat("dd MMM yyyy HH:mm:ss.SSS", Locale.ENGLISH);
    dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));

    sb.append(dateFormat.format(timestamp));

    if (message != null) {
      sb.append(' ').append(message);
      if (trace != null) {
        sb.append('\n').append(trace);
      }
    }
    else {
      if (trace != null) {
        sb.append(' ').append(trace);
      }
    }

    return sb.toString();
  }
}
