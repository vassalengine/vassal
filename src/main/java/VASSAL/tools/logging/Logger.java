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

import java.util.concurrent.Future;

import VASSAL.Info;
import VASSAL.tools.concurrent.SimpleFuture;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 * @deprecated Use {@link org.slf4j.Logger} instead.
 */
@Deprecated
public class Logger {
  private Logger() {}

  public static final int MESSAGE = LogEntry.MESSAGE;
  public static final int WARNING = LogEntry.WARNING;
  public static final int ERROR =   LogEntry.ERROR;
  public static final int BUG =     LogEntry.BUG;
  public static final int DEBUG =   LogEntry.DEBUG;
  public static final int SYSTEM =  LogEntry.SYSTEM;

  private static final int pid = Info.getInstanceID();

  public static void log(String message) {
    log(null, message, MESSAGE);
  }

  public static void log(Throwable thrown) {
    log(thrown, null, ERROR);
  }

  public static void log(Throwable thrown, String message) {
    log(thrown, message, ERROR);
  }

  public static void log(String message, int type) {
    log(null, message, type);
  }

  public static void log(Throwable thrown, int type) {
    log(thrown, null, type);
  }

  public static void log(Throwable thrown, String message, int type) {
    enqueue(new LogEntry(pid, type, thrown, message, false));
  }

  public static Future<?> logAndWait(String message) {
    return logAndWait(null, message, MESSAGE);
  }

  public static Future<?> logAndWait(Throwable thrown) {
    return logAndWait(thrown, null, ERROR);
  }

  public static Future<?> logAndWait(Throwable thrown, String message) {
    return logAndWait(thrown, message, ERROR);
  }

  public static Future<?> logAndWait(String message, int type) {
    return logAndWait(null, message, type);
  }

  public static Future<?> logAndWait(Throwable thrown, int type) {
    return logAndWait(thrown, null, type);
  }

  public static Future<?> logAndWait(Throwable thrown,
                                     String message, int type) {
    return enqueue(new LogEntry(pid, type, thrown, message, true));
  }

  public static Future<?> enqueue(final LogEntry entry) {
    final SimpleFuture<?> f = new SimpleFuture<Void>();
    f.set(null);
    return f;
  }

  public static void addLogListener(LogListener l) {
  }

  public static void removeLogListener(LogListener l) {
  }

  public static LogListener[] getLogListeners() {
    return new LogListener[0];
  }
}
