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

import java.io.PrintWriter;
import java.io.StringWriter;

import VASSAL.tools.logging.Logger;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ThrowableUtils {
  private ThrowableUtils() {}

    /**
   * Finds a {@link Throwable} of class <code>T</code> in the causal
   * history of the given <code>Throwable</code>, if one exists.
   *
   * @param cl the {@link Class} to search for
   * @param t  the <code>Throwable</code> to check
   * @return the ancestor of class <code>T</code>, or <code>null</code>
   * if none exists
   */
  public static <T extends Throwable> T getAncestorOfClass(Class<T> cl,
                                                           Throwable t) {
    // traverse the causal history of t until a cause of type cl is found
    for (Throwable c = t.getCause(); c != null; c = c.getCause()) {
      if (cl.isInstance(c)) return cl.cast(c);
    }

    return null;
  }
 
  /**
   * Throws a {@link Throwable} of class <code>T</code> if the
   * given <code>Throwable</code> has an ancestor of class <code>T</code>.
   *
   * @param cl the <code>Class</code> to search for
   * @param t the <code>Throwable</code> to check
   * @throws T if an ancestor of that class is found
   */
  public static <T extends Throwable> void throwAncestorOfClass(
      Class<T> cl, Throwable t) throws T {
    final T ancestor = getAncestorOfClass(cl, t);
    if (ancestor != null) {
      T toThrow = null;
      try {
        toThrow = cl.cast(cl.getConstructor().newInstance().initCause(t));
      }
      catch (Throwable ignore) {
        // If anything happens here, we're screwed anyway, since we're
        // already calling this during error handling. Just log it and
        // fight on.
        Logger.log(ignore);
      }

      if (toThrow != null) throw toThrow;
    }
  }

  /**
   * Converts a {@link Throwable}'s stack trace to a {@link String}.
   *
   * @param thrown the <code>Throwable</code> with the stack trace to convert
   * @return the stack trace as a <code>String</code>
   */
  public static String getStackTrace(Throwable thrown) {
    final StringWriter sw = new StringWriter();
    final PrintWriter pw = new PrintWriter(sw);
    thrown.printStackTrace(pw);
    pw.flush();
    return sw.toString();
  }      
}
