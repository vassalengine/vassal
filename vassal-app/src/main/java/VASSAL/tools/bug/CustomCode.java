/*
 *
 * Copyright (c) 2026 by the VASSAL Development Team
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

import VASSAL.tools.DataArchive;

/**
 * Decides whether a {@link Throwable} came out of custom Java code supplied by a module rather
 * than out of VASSAL.
 *
 * <p>A module may ship compiled classes inside its archive, and VASSAL loads them through the
 * module's {@link DataArchive}, which is itself a {@link ClassLoader}. When such a class throws,
 * the failure is the module's to answer for: the VASSAL developers cannot fix code they did not
 * write and do not have. Reporting it as a VASSAL bug tells the player the wrong thing and sends
 * the report to the wrong people.</p>
 *
 * <p>The test is the name of the class loader which defined the frame's class. The VM records that
 * name in every {@link StackTraceElement} it fills in, so it is available from the throwable alone,
 * with no need to resolve class names or to keep a registry of what has been loaded.</p>
 */
public final class CustomCode {
  private CustomCode() {}

  /**
   * Class name prefixes of code which is neither VASSAL's nor a module's: the Java platform
   * itself. A failure frequently throws from inside a platform class called by the code actually
   * at fault -- {@code Objects.requireNonNull}, a collection accessor, a reflective invocation --
   * so these frames are skipped when looking for whose code failed.
   */
  private static final String[] PLATFORM = {
    "java.", "javax.", "jdk.", "sun.", "com.sun." //NON-NLS
  };

  /**
   * Names the module-supplied class which a throwable came out of, if it came out of one.
   *
   * <p>The throwable is followed to its root cause, since that is where the failure originated,
   * and the first frame of that cause which is not in the Java platform decides: if the class in
   * that frame was loaded out of a module or extension archive the module is at fault, and if it
   * was not -- a VASSAL class, or one of the libraries VASSAL bundles -- then it is not. Note that
   * this deliberately blames the module only when its own code is the innermost non-platform
   * frame. A VASSAL class which fails while called from module code is still VASSAL's bug.</p>
   *
   * @param thrown the throwable to attribute
   * @return the name of the module-supplied class which failed, or null if none did
   */
  public static String blame(Throwable thrown) {
    if (thrown == null) {
      return null;
    }

    for (final StackTraceElement frame : rootCause(thrown).getStackTrace()) {
      final String cname = frame.getClassName();
      if (isPlatform(cname)) {
        continue;
      }

      final String loader = frame.getClassLoaderName();
      return loader != null && loader.startsWith(DataArchive.MODULE_LOADER_PREFIX) ?
        cname : null;
    }

    return null;
  }

  /**
   * @param thrown a throwable
   * @return the innermost cause of {@code thrown}, or {@code thrown} itself if it has none
   */
  public static Throwable rootCause(Throwable thrown) {
    Throwable t = thrown;
    // Guard against a cycle: a throwable may be its own cause, and a chain may close on itself.
    for (int i = 0; i < 64; ++i) {
      final Throwable cause = t.getCause();
      if (cause == null || cause == t) {
        break;
      }
      t = cause;
    }
    return t;
  }

  private static boolean isPlatform(String cname) {
    for (final String p : PLATFORM) {
      if (cname.startsWith(p)) {
        return true;
      }
    }
    return false;
  }
}
