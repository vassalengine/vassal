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

import VASSAL.i18n.Resources;

/**
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
      ErrorDialog.warning(
        Resources.getString("Error.file_not_found"),
        Resources.getString("Error.file_not_found"),
        t,
        (Object) (e.getClass().getName() + "@" + filename),
        Resources.getString("Error.file_not_found_message", filename)
      );
    }
    else { 
      // something more serious happened during I/O
      ErrorDialog.error(
        Resources.getString("Error.file_read_error"),
        Resources.getString("Error.file_read_error"),
        t,
        (Object) (e.getClass().getName() + "@" + filename),
        Resources.getString("Error.file_read_error_message", filename)
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
}
