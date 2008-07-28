/*
 * $Id$
 *
 * Copyright (c) 2000-2003, 2008 by Rodney Kinney, Joel Uckelman
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

package VASSAL.tools;

import VASSAL.Info;

/**
 * Logs errors when problems have occurred.
 */
public class ErrorLog implements Thread.UncaughtExceptionHandler {

  // Sets this class to handle exceptions occurring on the EDT.
  // See Java code in {@link EventDispatchThread.handleException()}.
  // In the case of an exception, the class is instantiated using its
  // zero-argument constructor and {@link handle()} is called with the
  // Throwable from the EDT.
  static {
    System.getProperties().put("sun.awt.exception.handler", //$NON-NLS-1$
                               ErrorLog.class.getName());
  }

  public ErrorLog() { }

  /**
   * Handles uncaught exceptions. Displays a {@link BugDialog} for
   * exceptions other than {@link OutOfMemoryException}.
   *
   * @param t the <code>Throwable</code> which was not caught.
   */ 
  public void handle(Throwable t) {
    ErrorDialog.bug(t);
  }

  /**
   * @inheritDocs
   * 
   * Needed to implement {@link Thread.UncaughtExceptionHandler}. Do
   * not call this method directly.
   */
  public void uncaughtException(Thread thread, Throwable t) {
    handle(t);
  }

  /**
   * Write a stack trace to the log.
   *
   * @param t the <code>Throwable</code> holding the stack trace
   */
  public static void log(Throwable t) {
    System.err.println("[" + Info.getInstanceID() + "]");
    t.printStackTrace();
  }

  /**
   * Write a message to the log.
   *
   * @param message the message to log
   */ 
  public static void log(String message) {
    System.err.println("[" + Info.getInstanceID() + "]");
    System.err.println(message);
  }

  public static void main(String[] args) {
    final ErrorLog log = new ErrorLog();
    log.handle(new RuntimeException("Error!!!"));
    System.exit(0);
  }
}
