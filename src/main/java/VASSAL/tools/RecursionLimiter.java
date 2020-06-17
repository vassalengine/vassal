/*
 * $Id$
 *
 * Copyright (c) 2009 by Brent Easton
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

/**
 * Utility class to detect and report Infinite Loops within
 * Vassal components
 *
 */
public class RecursionLimiter {
  protected static final int RECURSION_LIMIT = 50;
  protected static int recursionDepth = 0;
  protected static boolean reporting = false;

  public static void startExecution(Loopable looper)
                                              throws RecursionLimitException {
    if (++recursionDepth > RECURSION_LIMIT) {
      throw new RecursionLimitException(looper);
    }
  }

  public static void endExecution() {
    --recursionDepth;
  }

  /**
   * Report an Infinite Loop
   * @param e
   */
  public static void infiniteLoop(RecursionLimitException e) {
    reporting = true;
    ErrorDialog.showDetails(
      e,
      ThrowableUtils.getStackTrace(e),
      "Error.infinite_loop",
      e.getComponentTypeName(),
      e.getComponentName()
    );
    reporting = false;
  }
  
  public static boolean isReportingInfiniteLoop() {
    return reporting;
  }

  public static interface Loopable {
    public String getComponentTypeName();
    public String getComponentName();
  }
}
