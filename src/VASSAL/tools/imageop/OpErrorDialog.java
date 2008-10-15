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

import java.io.FileNotFoundException;
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
// FIXME!
      if (ioe instanceof FileNotFoundException) {
        // the file doesn't exist
        final SourceOp sop = findSourceOp(op);
        final String filename = sop != null ? sop.getName() : "";

        ErrorDialog.dataError(
          new BadDataReport("Image not found", filename, ioe));

        ErrorDialog.error(
          Resources.getString("Error.file_not_found"),
          Resources.getString("Error.file_not_found"),
          e,
          Resources.getString("Error.file_not_found_message", filename),
          ioe.getMessage()
        );
      }
      else {
        // otherwise we had some other kind of I/O problem
        ErrorDialog.error(
          Resources.getString("Error.io_error"),
          Resources.getString("Error.io_error"),
          e,
          Resources.getString("Error.io_error_message"),
          ioe.getMessage()
        );
      }
    }
    else {
      // otherwise this is a bug
      ErrorDialog.bug(e);
    }
  }

  private static SourceOp findSourceOp(VASSAL.tools.opcache.Op<?> op) {
    if (op instanceof SourceOp) return (SourceOp) op;
    
    for (VASSAL.tools.opcache.Op<?> src : op.getSources()) {
      final SourceOp sop = findSourceOp(src);
      if (sop != null) return sop;
    }
    
    return null;
  }
}
