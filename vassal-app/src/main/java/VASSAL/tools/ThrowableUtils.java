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

import java.io.PrintWriter;
import java.io.StringWriter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ThrowableUtils {
  private ThrowableUtils() {}

  private static final Logger logger =
    LoggerFactory.getLogger(ThrowableUtils.class);

  /**
   * Returns the most recent {@link Throwable} of class <code>T</code> in
   * the proper causal history of the given <code>Throwable</code>, if one
   * exists.
   *
   * @param cl the {@link Class} to search for
   * @param t  the <code>Throwable</code> to check
   * @return the proper ancestor of class <code>T</code>, or <code>null</code>
   * if none exists
   */
  public static <T extends Throwable> T getAncestor(Class<T> cl, Throwable t) {
    // traverse the causal history of t until a cause of type cl is found
    for (Throwable c = t.getCause(); c != null; c = c.getCause()) {
      if (cl.isInstance(c)) return cl.cast(c);
    }

    return null;
  }

  /**
   * Returns the most recent {@link Throwable} of class <code>T</code> in
   * the (not necessarily proper) causal history of the given
   * <code>Throwable</code>, if one exists. If the given
   * <code>Throwable</code> is of class <code>T</code>, it will be returned.
   *
   * @param cl the {@link Class} to search for
   * @param t  the <code>Throwable</code> to check
   * @return the ancestor of class <code>T</code>, or <code>null</code>
   * if none exists
   */
  public static <T extends Throwable> T getRecent(Class<T> cl, Throwable t) {
    if (cl.isInstance(t)) return cl.cast(t);
    return getAncestor(cl, t);
  }

  /**
   * Throws the most recent {@link Throwable} of class <code>T</code> in
   * the proper causal history of the given <code>Throwable</code>, if one
   * exists.
   *
   * @param cl the <code>Class</code> to search for
   * @param t the <code>Throwable</code> to check
   * @throws T if an ancestor of that class is found
   */
  public static <T extends Throwable> void throwAncestor(
      Class<T> cl, Throwable t) throws T {
    final T ancestor = getAncestor(cl, t);
    if (ancestor != null) throwMe(cl, ancestor);
  }

  /**
   * Throws the most recent {@link Throwable} of class <code>T</code> in
   * the (not necessarily proper) causal history of the given
   * <code>Throwable</code>, if one exists.
   *
   * @param cl the <code>Class</code> to search for
   * @param t the <code>Throwable</code> to check
   * @throws T if an ancestor of that class is found
   */
  public static <T extends Throwable> void throwRecent(Class<T> cl,
                                                       Throwable t) throws T {
    if (cl.isInstance(t)) throwMe(cl, t);
    else throwAncestor(cl, t);
  }

  private static <T extends Throwable> void throwMe(Class<T> cl, Throwable t)
                                                                     throws T {
    T toThrow = null;
    try {
      toThrow = cl.cast(cl.getConstructor().newInstance().initCause(t));
    }
    catch (Throwable ignore) {
      // If anything happens here, we're screwed anyway, as we're already
      // calling this during error handling. Just log it and soldier on.
      logger.warn("ignored", ignore);  //NON-NLS
    }

    if (toThrow != null) throw toThrow;
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
