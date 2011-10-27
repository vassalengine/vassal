/*
 * $Id$
 *
 * Copyright (c) 2011 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.bug;

import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;

public class Bug2694Handler implements BugHandler {
  public boolean accept(Throwable thrown) {
    if (thrown instanceof UnsatisfiedLinkError) {
      final StackTraceElement[] trace = thrown.getStackTrace();
      if (trace.length > 0) {
        final StackTraceElement e = trace[0];
        if ("sun.awt.image.ImageRepresentation".equals(e.getClassName()) &&
            "setBytePixels".equals(e.getMethodName())) {
          return true;
        }
      }
    }

    return false;
  }

  public void handle(Throwable thrown) {
    ErrorDialog.showDetails(
      thrown, ThrowableUtils.getStackTrace(thrown), "Error.bug2694"
    );
  }
}
