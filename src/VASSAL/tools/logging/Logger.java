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

import VASSAL.Info;

import java.util.concurrent.CountDownLatch;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class Logger {
  private Logger() {}

  public static final int MESSAGE = LogEntry.MESSAGE;
  public static final int WARNING = LogEntry.WARNING;
  public static final int ERROR =   LogEntry.ERROR;
  public static final int BUG =     LogEntry.BUG;
  public static final int DEBUG =   LogEntry.DEBUG;
  public static final int SYSTEM =  LogEntry.SYSTEM;

  private static final long pid = Info.getInstanceID();

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
    LogManager.enqueue(new LogEntry(pid, type, thrown, message));
  }

 public static void logAndWait(String message) {
    logAndWait(null, message, MESSAGE);
  }

  public static void logAndWait(Throwable thrown) {
    logAndWait(thrown, null, ERROR);
  }

  public static void logAndWait(Throwable thrown, String message) {
    logAndWait(thrown, message, ERROR);
  }

  public static void logAndWait(String message, int type) {
    logAndWait(null, message, type);
  }

  public static void logAndWait(Throwable thrown, int type) {
    logAndWait(thrown, null, type);
  }

  public static void logAndWait(Throwable thrown, String message, int type) {
    log(thrown, message, type);
    LogManager.flush();
  }
}
