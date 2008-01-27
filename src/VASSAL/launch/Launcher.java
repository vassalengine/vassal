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

package VASSAL.launch;

import java.io.IOException;

import VASSAL.build.module.GlobalOptions;
import VASSAL.preferences.Prefs;

/**
 * Spawns a child JVM to run VASSAL with the user's desired initial
 * and maximum heap.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class Launcher {
  public static final int DEFAULT_INITIAL_HEAP = 256;
  public static final int DEFAULT_MAXIMUM_HEAP = 512;

  /**
   * @param args arguments which are passed through to
   * {@link VASSAL.launch.Main.main(String[])}
   */
  public static void main(String[] args) {
    // try to get initial and maximum heap from user preferences
    int initialHeap; 
    try {
      initialHeap = Integer.parseInt(
        Prefs.getGlobalPrefs().getStoredValue(GlobalOptions.INITIAL_HEAP));
    }
    catch (NumberFormatException e) {
      e.printStackTrace();
      initialHeap = DEFAULT_INITIAL_HEAP;
    }

    int maximumHeap;
    try {
      maximumHeap = Integer.parseInt(
        Prefs.getGlobalPrefs().getStoredValue(GlobalOptions.MAXIMUM_HEAP));
    }
    catch (NumberFormatException e) {
      e.printStackTrace();
      maximumHeap = DEFAULT_MAXIMUM_HEAP;
    }

    // build the initial part of the launch command 
    final String[] jcmd;
    if (System.getProperty("os.name").toLowerCase().startsWith("windows")) {
      jcmd = new String[] {
        "javaw",
        "-client",
        "-Xms" + initialHeap + "M",
        "-Xmx" + maximumHeap + "M",
        "-classpath",
        "lib/Vengine.jar",
//      "-Dsun.java2d.opengl=True",
        "VASSAL.launch.Main",
        "-extract",
        "/docsInfo"
      };
    }
    else {
      jcmd = new String[] {
        "java",
        "-Xms" + initialHeap + "M",
        "-Xmx" + maximumHeap + "M",
        "-classpath",
        "lib/Vengine.jar",
//      "-Dsun.java2d.opengl=True",
        "VASSAL.launch.Main",
        "-extract",
        "/docsInfo"
      };
    }

    // pass through any arguments we received
    final String[] cmd = new String[jcmd.length + args.length];
    System.arraycopy(jcmd, 0, cmd, 0, jcmd.length);
    System.arraycopy(args, 0, cmd, jcmd.length, args.length);

    // launch VASSAL
    try {
      new ProcessBuilder(cmd).start();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    System.exit(0);
  }
}
