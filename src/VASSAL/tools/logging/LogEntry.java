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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.logging;

import java.io.Serializable;

import VASSAL.tools.BugUtils;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class LogEntry implements Serializable {
  private static final long serialVersionUID = 1L;

  public static final int MESSAGE = 0;
  public static final int WARNING = 1;
  public static final int ERROR = 2;
  public static final int BUG = 3;
  public static final int DEBUG = 4;
  public static final int SYSTEM = 5;

  public final long timestamp;
  public final long pid;
  public final int type;
  public final Throwable thrown;
  public final String message;

  public LogEntry(long pid, int type, Throwable thrown, String message) {
    this(System.currentTimeMillis(), pid, type, thrown, message);
  }

  public LogEntry(long timestamp, long pid, int type,
                  Throwable thrown, String message) {

    if (thrown == null && message == null)
      throw new IllegalArgumentException();

    this.timestamp = timestamp;
    this.pid = pid;
    this.type = type;
    this.thrown = thrown;
    this.message = message;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder();
    sb.append(timestamp).append(' ').append(pid);
    if (message != null) sb.append(' ').append(message);
    if (thrown  != null) sb.append(' ').append(BugUtils.getStackTrace(thrown));
    return sb.toString();
  }
}
