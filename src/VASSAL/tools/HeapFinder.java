/*
 * $Id: AbstractLaunchAction.java 3939 2008-07-30 20:47:37Z uckelman $
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

import java.io.IOException;

import VASSAL.Info;

public class HeapFinder {
  private HeapFinder() {}

  public static int getMaxMaxHeap() {
    // Fail quickly on a 64-bit JVM: 32-bit machines have only 4096MB
    // of address space. Sun's 64-bit JVMs permit the maximum heap to
    // be set to vastly more memory than the host machine could have,
    // so searching for the maximum maximum heap is pointless there.
    if (runJVM(4097) == 0) return -1;

    // Else we are 32-bit. Working down from 4096 is likely to be more
    // efficient than working upwards: Most systems will have at least
    // 1024MB of RAM, and so we'll get an upper bound in no more than
    // three steps.
    int maxheap = 4096;
    
    // Get a lower bound on maximum heap by halving until the JVM succeeds.
    for (int ret = -1; ret != 0 && maxheap > 0; maxheap >>= 1) {
      ret = runJVM(maxheap);
    }

    // maxheap is now our lower bound; the upper bound is twice that.
    maxheap <<= 1;

    // Now work upwards from our lower bound by binary search.
    for (int delta = maxheap >> 1; delta > 0; delta >>= 1) {
      if (runJVM(maxheap + delta) == 0) maxheap += delta;
    }

    return maxheap;
  }

  private static int runJVM(int maxheap) {
    int ret = 0;

    try {
      final Process p = new ProcessBuilder(
        Info.javaBinPath,
        "-Xmx" + maxheap + "M",
        "-cp",
        System.getProperty("java.class.path"),
        "VASSAL.tools.HeapFinderDummy"
      ).start();

      try { 
        ret = p.waitFor();

//        System.err.println("retval = " + ret + ", maxheap = " + maxheap);
      }
      catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
    catch (IOException e) {  
      e.printStackTrace();
    }

    return ret;
  }
 
  public static void main(String[] args) {
    System.out.println("getMaxMaxHeap() = " + getMaxMaxHeap());
  } 
}
