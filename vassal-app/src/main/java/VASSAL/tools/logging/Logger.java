/*
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

import VASSAL.tools.concurrent.SimpleFuture;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 * @deprecated Use {@link org.slf4j.Logger} instead.
 */
@Deprecated(since = "2021-08-06", forRemoval = true)
public class Logger {
  private Logger() {}

  @SuppressWarnings({"deprecation", "removal"})
  public static final int MESSAGE = LogEntry.MESSAGE;
  @SuppressWarnings({"deprecation", "removal"})
  public static final int WARNING = LogEntry.WARNING;
  @SuppressWarnings({"deprecation", "removal"})
  public static final int ERROR =   LogEntry.ERROR;
  @SuppressWarnings({"deprecation", "removal"})
  public static final int BUG =     LogEntry.BUG;
  @SuppressWarnings({"deprecation", "removal"})
  public static final int DEBUG =   LogEntry.DEBUG;
  @SuppressWarnings({"deprecation", "removal"})
  public static final int SYSTEM =  LogEntry.SYSTEM;

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

  @SuppressWarnings({"deprecation", "removal"})
  public static void log(Throwable thrown, String message, int type) {
    enqueue(new LogEntry(type, thrown, message, false));
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

  @SuppressWarnings({"deprecation", "removal"})
  public static Future<?> logAndWait(Throwable thrown,
                                     String message, int type) {
    return enqueue(new LogEntry(type, thrown, message, true));
  }

  @SuppressWarnings({"deprecation", "removal"})
  public static Future<?> enqueue(final LogEntry entry) {
    final SimpleFuture<?> f = new SimpleFuture<>();
    f.set(null);
    return f;
  }

  @SuppressWarnings({"deprecation", "removal"})
  public static void addLogListener(LogListener l) {
  }

  @SuppressWarnings({"deprecation", "removal"})
  public static void removeLogListener(LogListener l) {
  }

  @SuppressWarnings({"deprecation", "removal"})
  public static LogListener[] getLogListeners() {
    return new LogListener[0];
  }
}
