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

import java.io.IOException;

import VASSAL.i18n.Resources;

/**
 * An error dialog for indicating a communication error between the
 * Module Manager and a child process.
 *
 * What we've got here is a failure to communicate.<br>
 * &emdash;warden, <em>Cool Hand Luke</em> (19xx)
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class CommunicationErrorDialog {
  private CommunicationErrorDialog() {}

  /**
   * @param t the <code>Throwable</code> providing the stack trace
   * @param e the communication exception
   */
  public static void error(Throwable t, IOException e) {
    ErrorDialog.error(
      Resources.getString("Error.communication_error"),
      Resources.getString("Error.communication_error"),
      t,
      Resources.getString("Error.communication_error")
    );
  }

  /**
   * @param e the communication exception
   */
  public static void error(IOException e) {
    error(e, e);
  }
}
