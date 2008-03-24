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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

import VASSAL.Info;
import VASSAL.build.module.GlobalOptions;
import VASSAL.preferences.Prefs;
import VASSAL.tools.IOUtils;

/**
 * Writes initial and minimum heap sizes in a platform-specific way.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public abstract class HeapSetter {
  public static final int DEFAULT_INITIAL_HEAP = 256;
  public static final int DEFAULT_MAXIMUM_HEAP = 512;

  private static final HeapSetter instance;

  static {
    if      (Info.isMacOSX())  instance = new MacOSXHeapSetter();
    else if (Info.isWindows()) instance = new WindowsHeapSetter();
    else                       instance = new UnixHeapSetter();
  }

  public static HeapSetter getInstance() {
    return instance;
  }

  /**
   * Invoke this program to create a heap settings file from the
   * user's preferences. This program is run by the Windows installer
   * to create a heaps file prior to running VASSAL.
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

    // write them out to the system-appropriate heaps file
    try {
      HeapSetter.getInstance().setHeaps(initialHeap, maximumHeap);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * Set initial and minimum heap sizes.
   *
   * @param initial the initial heap, in megabytes
   * @param initial the maximum heap, in megabytes
   */
  public abstract void setHeaps(int initial, int maximum) throws IOException;

  private static class UnixHeapSetter extends HeapSetter {
    public void setHeaps(int initial, int maximum) throws IOException {
      // heaps format: space-separated list of options

      // We are running on some UNIX. Hooray! The plan here is to
      // update the heaps file, which will be cat'ed in as an argument
      // to the JVM in the VASSAL.sh shell script.

      final File file = new File(Info.getBaseDir(), "heaps");
      final Writer out = new BufferedWriter(new FileWriter(file));
      try {
        out.write("-Xms" + initial + "M -Xmx" + maximum + "M");
      }
      finally {
        out.close();
      }
    } 
  }

  private static class WindowsHeapSetter extends HeapSetter {
    public void setHeaps(int initial, int maximum) throws IOException {
      // We are running on Windows. Blech! The plan here is to update
      // the VASSAL.l4j.ini which the Launch4j JAR wrapper reads.

      // Launch4j INI format: one option per line, # marks comments 

      final File file = new File(Info.getBaseDir(), "VASSAL.l4j.ini");

      String s = null;
      try {
        final Reader in = new BufferedReader(new FileReader(file));
        try {
          s = IOUtils.toString(in);
        }
        finally {
          in.close();
        }
      }
      catch (FileNotFoundException e) {
        // write a new VASSAL.l4j.ini if there is no old one
      }

      final String iHeap = "-Xms" + initial + "M";
      final String mHeap = "-Xmx" + maximum + "M";

      if (s != null) {
        s = s.replaceFirst("-Xms\\w*", iHeap)
             .replaceFirst("-Xmx\\w*", mHeap);
      }
      else {
        s = iHeap + "\n" + mHeap + "\n";
      }

      final Writer out = new BufferedWriter(new FileWriter(file));
      try {
        out.write(s);
      }
      finally {
        out.close();
      }
    }
  }

  private static class MacOSXHeapSetter extends HeapSetter {
    public void setHeaps(int initial, int maximum) throws IOException {
      // Info.plist fomat: look on the internet
 
      // We are running on OSX. The plan here is to update the
      // Info.plist which is contained in the VASSAL.app bundle.
 
      final File file = new File(Info.getBaseDir(), "Contents/Info.plist");
      final String VMOptionsKey = "<key>VMOptions</key>";
  
      String s = null;
      final Reader in = new BufferedReader(new FileReader(file));
      try {
        s = IOUtils.toString(in);
      }
      finally {
        in.close();
      }
  
      final String iHeap = "-Xms" + initial + "M";
      final String mHeap = "-Xmx" + maximum + "M";
  
      // Replace only after VMOptions, since it's possible, though
      // unlikely, that "-Xms" or "-Xmx" appears somewhere else in
      // the Info.plist.
      final int i = s.indexOf(VMOptionsKey) + VMOptionsKey.length();
      s = s.substring(0, i) +
          s.substring(i)
           .replaceFirst("-Xms\\w*", iHeap)
           .replaceFirst("-Xmx\\w*", mHeap);
  
      final Writer out = new BufferedWriter(new FileWriter(file));
      try {
        out.write(s);
      }
      finally {
        out.close();
      }
    }
  }
}
