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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.launch;

import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.DONE;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.READY;

/**
 * Stands in for {@code ZipFileImageTiler} in {@link TilingHandlerRetryTest}, speaking enough of
 * its protocol to exercise {@link TilingHandler#runSlicer}. Run as a real subprocess, so that
 * what is tested is what happens to a real child process.
 *
 * <p>Modes:</p>
 * <dl>
 *   <dt>{@code never-starts}</dt><dd>prints an error and exits 1 without announcing itself,
 *   as a JVM does when it cannot load the main class</dd>
 *   <dt>{@code oom}</dt><dd>announces itself and exits 2, as the real tiler does when an
 *   OutOfMemoryError stopped it</dd>
 *   <dt>{@code other-failure}</dt><dd>announces itself and exits 1</dd>
 *   <dt>{@code success}</dt><dd>announces itself, finishes and exits 0</dd>
 * </dl>
 */
public final class FakeTiler {
  private FakeTiler() {
  }

  public static void main(String[] args) {
    switch (args[0]) {
    case "never-starts": //NON-NLS
      System.err.println("Error: Could not find or load main class SomeTiler"); //NON-NLS
      System.exit(1);
      break;

    case "oom": //NON-NLS
      System.out.println(READY);
      System.out.flush();
      System.exit(2);
      break;

    case "other-failure": //NON-NLS
      System.out.println(READY);
      System.out.flush();
      System.exit(1);
      break;

    default:
      System.out.println(READY);
      System.out.println(DONE);
      System.out.flush();
      System.exit(0);
    }
  }
}
