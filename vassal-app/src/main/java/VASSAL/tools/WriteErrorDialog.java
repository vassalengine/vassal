/*
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

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.nio.file.FileSystemException;


import javax.swing.JOptionPane;

import VASSAL.i18n.Resources;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class WriteErrorDialog {
  private WriteErrorDialog() {}

  /**
   * @param t the <code>Throwable</code> providing the stack trace
   * @param e the write exception
   * @param filename the file which was being written
   */
  public static void error(Throwable t, IOException e, String filename) {
    ErrorDialog.showDetails(
      t,
      ThrowableUtils.getStackTrace(e),
      "Error.file_write_error", //NON-NLS
      filename
    );
  }

  /**
   * @param t the <code>Throwable</code> providing the stack trace
   * @param e the write exception
   * @param file the file which was being written
   */
  public static void error(Throwable t, IOException e, File file) {
    error(t, e, file.getPath());
  }

  /**
   * @param e the write exception
   * @param filename the file which was being written
   */
  public static void error(IOException e, String filename) {
    error(e, e, filename);
  }

  /**
   * @param e the write exception
   * @param file the file which was being written
   */
  public static void error(IOException e, File file) {
    error(e, e, file.getPath());
  }

  /**
   * These presently come from failure to replace a module file with a new version
   * @param e
   */
  public static void reportFileOverwriteFailure(FileSystemException e, String messageKey) {
    ErrorDialog.showFileOverwriteFailure(
      e,
      ThrowableUtils.getStackTrace(e),
      messageKey
    );
  }

  public static void showError(
    Component parent,
    IOException e,
    File file,
    String messageKey) {

    final String msg;
    if (e instanceof AccessDeniedException) {
      // AccessDeniedException has a useless message
      msg = "Access denied";
    }
    else if (e instanceof FileSystemException) {
      msg = ((FileSystemException) e).getReason();
    }
    else {
      msg = e.getMessage();
    }

    ProblemDialog.showDetails(
      JOptionPane.WARNING_MESSAGE,
      parent,
      e,
      ThrowableUtils.getStackTrace(e),
      Resources.getString(messageKey + "_title"), // NON-NLS
      Resources.getString(messageKey + "_heading"), // NON-NLS
      Resources.getString(messageKey + "_message", file.getName(), msg) // NON-NLS
    );
  }
}
