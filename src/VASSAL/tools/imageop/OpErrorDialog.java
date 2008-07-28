/*
 * $Id: SourceOpSVGImpl.java 3665 2008-05-26 21:02:09Z uckelman $
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

import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ReadErrorDialog;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class OpErrorDialog {
  private OpErrorDialog() {}

  public static void error(ExecutionException e, ImageOp op) {
    // unpack the exception
    for (Throwable c = e; c != null; c = c.getCause()) {
      if (c instanceof IOException) {
        for (ImageOp src = op; src != null; src = src.getSource()) {
          if (src instanceof SourceOp) {
            // we had a file read error
            ReadErrorDialog.error(e, (IOException) c,
                                  ((SourceOp) src).getName());
            return;
          }
        }

        // otherwise we had some other kind of I/O problem
        ErrorDialog.error(
          Resources.getString("Error.io_error"),
          Resources.getString("Error.io_error"),
          e,
          Resources.getString("IOErrorDialog.io_error"),
          c.getMessage()
        );

        return;
      }
    }

    // otherwise this is a bug
    ErrorDialog.bug(e);
  }
}
