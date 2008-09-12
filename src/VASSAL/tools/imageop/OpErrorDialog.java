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

package VASSAL.tools.imageop;

import java.io.IOException;
import java.util.concurrent.ExecutionException;

import VASSAL.build.BadDataReport;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ErrorUtils;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class OpErrorDialog {
  private OpErrorDialog() {}

  public static void error(ExecutionException e, ImageOp op) {
    // unpack the exception
    final IOException ioe = ErrorUtils.getAncestorOfClass(IOException.class, e);
    if (ioe != null) {
      for (ImageOp src = op; src != null; src = src.getSource()) {
        if (src instanceof SourceOp) {
          // Failed to find this image in the module
          ErrorDialog.dataError(new BadDataReport("Image not found",((SourceOp) src).getName(),ioe));
          return;
        }
      }

      // otherwise we had some other kind of I/O problem
      ErrorDialog.error(
        Resources.getString("Error.io_error"),
        Resources.getString("Error.io_error"),
        e,
        Resources.getString("Error.io_error_message"),
        ioe.getMessage()
      );

      return;
    }

    // otherwise this is a bug
    ErrorDialog.bug(e);
  }
}
