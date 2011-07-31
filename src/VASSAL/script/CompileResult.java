/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Brent Easton
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
package VASSAL.script;

import bsh.EvalError;
import bsh.ParseException;

public class CompileResult {
  protected boolean success;
  protected EvalError error = null;

  public CompileResult() {
    success = true;
  }

  public CompileResult(EvalError e) {
    success = false;
    error = e;
  }

  public boolean isSuccess() {
    return success;
  }

  public String getMessage() {
    String message = "";
    if (error != null) {
      if (error instanceof ParseException) {
        message = error.getMessage();
      }
      else {
        return error.getErrorLineNumber() + ": " + error.getErrorText();
      }
    }
    return message;
  }

  public void printStackTrace() {
    if (error != null) {
      error.printStackTrace();
    }
  }
}