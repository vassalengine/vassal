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

import java.util.concurrent.ExecutionException;

/**
 * Warns the user when an uncaught Exception occurs.
 * See Java code in {@link EventDispatchThread.handleException()}.
 */
public class ErrorLog {
  static {
    System.getProperties().put("sun.awt.exception.handler", //$NON-NLS-1$
                               "VASSAL.tools.ErrorLog");    //$NON-NLS-1$
  }

  public static void warn(Throwable t) {
    t.printStackTrace();

    // replace ExecutionExceptions with their causes
    if (t instanceof ExecutionException && t.getCause() != null)
      t = t.getCause();

    ErrorDialog.warning(t, t.getMessage());
  }

  public void handle(Throwable t) {
    t.printStackTrace();

    final String logFile = System.getProperty("stderr");  //$NON-NLS-1$
    if (!ErrorDialog.isDisabled(t.getClass())) {
      String text = null;

      if (t instanceof OutOfMemoryError) {
        final String s = t.getMessage();
        text = "The application has run out of memory.\n";
        if (s != null) {
          text += s+"\n";
        }
      }
      else {
        final String type = t.getClass().getSimpleName();
        String msg = t.getMessage();
        if (msg == null || msg.length() == 0) {
          msg = type;
        }
        else {
          msg = type + "\n" + msg;
        }

        text = "An untrapped error has occurred:\n\n" +
               msg + "\n\n" +
               "Please send a report to support@vassalengine.org and attach the log file.\n" + logFile;
      }

      ErrorDialog.error(t, text);
    }
  }

  public static void main(String[] args) {
    final ErrorLog log = new ErrorLog();
    while (!ErrorDialog.isDisabled(RuntimeException.class)) {
      log.handle(new RuntimeException("Warning!!!"));
    }
  }

  public static class Group extends ThreadGroup {
    private ErrorLog handler = new ErrorLog();

    public Group() {
      super("Main Thread");
    }

    public void uncaughtException(Thread t, Throwable e) {
      handler.handle(e);
    }
  }
}
