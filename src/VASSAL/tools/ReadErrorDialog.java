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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Utility class for reporting an IOException reading from the local system or a resource bundled with the VASSAL engine
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ReadErrorDialog {
  private ReadErrorDialog() {}

  /**
   * @param t the <code>Throwable</code> providing the stack trace
   * @param e the read exception
   * @param filename the file which was being read
   */
  public static void error(Throwable t, IOException e, String filename) {
    if (e instanceof FileNotFoundException) {
      // file is missing
      WarningDialog.showDisableable(
        t,
        (Object) (e.getClass().getName() + "@" + filename),
        "Error.file_not_found",
        filename
      );
    }
    else {
      // something more serious happened during I/O
      ErrorDialog.showDisableable(
        t,
        (Object) (e.getClass().getName() + "@" + filename),
        "Error.file_read_error",
        filename
      );
    }
  }

  /**
   * @param t the <code>Throwable</code> providing the stack trace
   * @param e the read exception
   * @param file the file which was being read
   */
  public static void error(Throwable t, IOException e, File file) {
    error(t, e, file.getPath());
  }

  /**
   * @param e the read exception
   * @param filename the file which was being read
   */
  public static void error(IOException e, String filename) {
    error(e, e, filename);
  }

  /**
   * @param e the read exception
   * @param file the file which was being read
   */
  public static void error(IOException e, File file) {
    error(e, e, file.getPath());
  }

  public static void errorNoI18N(IOException e, String filename) {
    ErrorDialog.showDisableable(
      null,
      e,
      (Object) (e.getClass().getName() + "@" + filename),
      "Unable to Read File",
      "Unable to Read File",
      "VASSAL was unable to read the file '" + filename + "'."
    );
  }

  public static void errorNoI18N(IOException e, File file) {
    errorNoI18N(e, file.getPath());
  }
}
