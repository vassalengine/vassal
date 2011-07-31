/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman
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
package VASSAL.launch;

import VASSAL.tools.ErrorDialog;

/**
 * Handles uncaught exceptions. None of the methods in this class are
 * intended to be called in our code, but must be public in order to be
 * accessible to {@link Thread} and {@link EventDispatchThread}.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ExceptionHandler implements Thread.UncaughtExceptionHandler {
  // Sets this class to handle exceptions occurring on the EDT.
  // See Java code in {@link EventDispatchThread.handleException()}.
  // In the case of an exception, the class is instantiated using its
  // zero-argument constructor and {@link handle()} is called with the
  // Throwable from the EDT.
  static {
    System.getProperties().put("sun.awt.exception.handler", //$NON-NLS-1$
                               ExceptionHandler.class.getName());
  }

  public ExceptionHandler() {}

  /**
   * Handles uncaught exceptions.
   *
   * @param thrown the <code>Throwable</code> which was not caught.
   */
  public void handle(Throwable thrown) {
    ErrorDialog.bug(thrown);
  }

  /**
   * {@inheritDoc}
   *
   * This method is part of the {@link Thread.UncaughtExceptionHandler}
   * interface. Do not call this method directly.
   *
   * @param thread the thread where the exception occurred
   * @param thrown the exception
   */
  public void uncaughtException(Thread thread, Throwable thrown) {
    handle(thrown);
  }
}
