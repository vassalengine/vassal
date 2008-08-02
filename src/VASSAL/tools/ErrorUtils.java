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

import java.lang.reflect.InvocationTargetException;

import VASSAL.i18n.Resources;

public class ErrorUtils {
  private ErrorUtils() {}

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
        ErrorLog.log(ignore);
      }

      if (toThrow != null) throw toThrow;
    }
  }

  public static void handleImportClassFailure(Throwable t, String className) {
    // find and rethrow causes which are not bugs
    throwAncestorOfClass(OutOfMemoryError.class, t);

    //
    // thrown by Class.forName()
    //
    if (t instanceof ClassNotFoundException) {
      ErrorDialog.error(
        Resources.getString("Error.class_not_found"),
        Resources.getString("Error.class_not_found"),
        t,
        Resources.getString("Error.class_not_found_message", className)
      );
    }
    else if (t instanceof ExceptionInInitializerError) {
      ErrorDialog.error(
        Resources.getString("Error.class_init_failed"),
        Resources.getString("Error.class_init_failed"),
        t,
        Resources.getString("Error.class_init_failed_message", className)
      );
    }
    else if (t instanceof LinkageError) {
      ErrorDialog.error(
        Resources.getString("Error.class_linkage_failed"),
        Resources.getString("Error.class_linkage_failed"),
        t,
        Resources.getString("Error.class_linkage_failed_message", className)
      );
    }
    //
    // thrown by Class.getConstructor()
    //
    else if (t instanceof NoSuchMethodException) {
      ErrorDialog.error(
        Resources.getString("Error.no_nullary_ctor"),
        Resources.getString("Error.no_nullary_ctor"),
        t,
        Resources.getString("Error.no_nullary_ctor_message", className)
      );
    }
    else if (t instanceof SecurityException) {
      
    }
    //
    // thrown by Constructor.newInstance()
    //
    else if (t instanceof IllegalAccessException) {
      ErrorDialog.error(
        Resources.getString("Error.nonpublic_ctor"),
        Resources.getString("Error.nonpublic_ctor"),
        t,
        Resources.getString("Error.nonpublic_ctor_message", className)
      );
    }
    else if (t instanceof IllegalArgumentException) {
      // This ought to be impossible, since the ctor is supposed to be
      // nullary and so should lack arguments altogether. If the ctor
      // isn't nullary, we should already have had a NoSuchMethodException.
      ErrorDialog.bug(t);
    }
    else if (t instanceof InstantiationException) {
      ErrorDialog.error(
        Resources.getString("Error.class_not_concrete"),
        Resources.getString("Error.class_not_concrete"),
        t,
        Resources.getString("Error.class_not_concrete_message", className)
      );
    }
    else if (t instanceof InvocationTargetException) {
      ErrorDialog.error(
        Resources.getString("Error.exception_in_ctor"),
        Resources.getString("Error.exception_in_ctor"),
        t,
        Resources.getString("Error.exception_in_ctor_message", className)
      );
    }
    //
    // extremal cases
    //
    else {
      // otherwise something weird happened
      if (t instanceof Error) {
        // some unusual problem occurred    
        throw (Error) t;
      }
      else if (t instanceof RuntimeException) {
        // some unusual problem occurred    
        throw (RuntimeException) t;
      }
      else {
        // this should never happen
        throw new IllegalStateException(t);
      }
    }
  }

  public static void handleClassLoadFailure(Throwable t) {
    // find and rethrow causes which are not bugs
    throwAncestorOfClass(OutOfMemoryError.class, t);

    handle(t, ClassNotFoundException.class,
              ExceptionInInitializerError.class,
              LinkageError.class
    );        
  }

  /**
   * Handle a {@link Throwable} generated by
   * <code>Class.getConstructor().newInstance()</code>.
   *
   * @param t the <code>Throwable</code>
   */ 
  public static void handleNewInstanceFailure(Throwable t) {
    // find and rethrow causes which are not bugs
    throwAncestorOfClass(OutOfMemoryError.class, t);

    handle(t, IllegalAccessException.class,
              IllegalArgumentException.class,
              InstantiationException.class,
              InvocationTargetException.class,
              NoSuchMethodException.class,
              SecurityException.class,
              ExceptionInInitializerError.class
    );
  }

  private static void handle(Throwable t, Class<?>... args) {
    // these errors are expected
    for (Class<?> cl : args) {
      if (cl.isInstance(t)) ErrorDialog.bug(t);
    }
  
    // otherwise something weird happened
    if (t instanceof Error) {
      // some unusual problem occurred    
      throw (Error) t;
    }
    else if (t instanceof RuntimeException) {
      // some unusual problem occurred    
      throw (RuntimeException) t;
    }
    else {
      // this should never happen
      throw new IllegalStateException(t);
    }
  }
}
