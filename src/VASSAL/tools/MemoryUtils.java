/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman 
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

import java.io.File;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.LongByReference;
import com.sun.jna.win32.StdCallLibrary;

public class MemoryUtils {
  private MemoryUtils() {}

  /**
   * Gets the amount of physical memory (RAM) in this machine, in bytes.
   *
   * @return the amount of RAM, in bytes
   */
  public static long getPhysicalMemory() {
    return IMPL.getPhysicalMemory();
  }

  private interface MemoryUtilsImpl {
    public long getPhysicalMemory();
  }

  private static final MemoryUtilsImpl IMPL;

  static {
    final String os = System.getProperty("os.name").toLowerCase();

    if (os.startsWith("linux")) {
      IMPL = new LinuxMemoryUtilsImpl();
    }
    else if (os.startsWith("mac os x")) {
      IMPL = new MacOSXMemoryUtilsImpl();
    }
    else if (os.startsWith("windows")) {
      IMPL = new WindowsMemoryUtilsImpl();
    }
    else {
      IMPL = new DummyMemoryUtilsImpl();
    }
  }

  private static class DummyMemoryUtilsImpl implements MemoryUtilsImpl {
    public long getPhysicalMemory() { return -1L; }
  }

  private static class LinuxMemoryUtilsImpl implements MemoryUtilsImpl{
    public long getPhysicalMemory() {
      // The file /proc/kcore corresponds to the physical RAM on Linux.
      // We use File.length() to stat it, giving us the RAM in bytes.
      return new File("/proc/kcore").length();
    }
  }

  private static class MacOSXMemoryUtilsImpl implements MemoryUtilsImpl {
    /**
    * The wrapper for libc.
    */
    public static interface Libc extends Library {
      /**
       *  Wrapper for sysctlbyname(3).
       *  
       *  See {@link http://developer.apple.com/documentation/Darwin/Reference/ManPages/man3/sysctl.3.html}
       */
      int sysctlbyname(String name, Pointer oldP, IntByReference oldlenp,
                                    Pointer newp, int newlen);

      /**
       * Wrapper for strerror(3).
       */
      String strerror(int errno);

      public static final Libc INSTANCE =
        (Libc) Native.loadLibrary("c", Libc.class);
    } 

    public long getPhysicalMemory() { 
      final LongByReference oldp = new LongByReference(0);
      final IntByReference oldlenp =
        new IntByReference(Native.getNativeSize(long.class));

      if (Libc.INSTANCE.sysctlbyname(
            "hw.memsize", 
            oldp.getPointer(), oldlenp,
            Pointer.NULL, 0) == 0)
      {
        return oldp.getValue();
      }
      else {
        final int errno = Native.getLastError();
        System.err.println(
          "Error " + errno + ": " + Libc.INSTANCE.strerror(errno));

        return -1L;
      }
    }
  }

  private static class WindowsMemoryUtilsImpl implements MemoryUtilsImpl {
    /**
     * The wrapper for Kernel32.dll.
     */
    public static interface Kernel32 extends StdCallLibrary {
      /**
       * Gets information about physical and virtual memory.
       *
       * See {@link
       * http://msdn.microsoft.com/en-us/library/aa366589(VS.85).aspx}
       * for further details about this function.
       *
       * @param p the structure where values are returned
       * @return <code>true</code> on success
       */
      boolean GlobalMemoryStatusEx(MEMORYSTATUSEX p);
      
      /**
       * Translates system error codes to error messages.
       *
       * See {@link
       * http://msdn.microsoft.com/en-us/library/ms679351(VS.85).aspx}
       * for further details about this function.
       */
      int FormatMessage(
        int dwFlags, Pointer lpSource, int dwMessageId, int dwLanguageId,
        String[] lpBuffer, int nSize, Pointer va_list);

      public static final int FORMAT_MESSAGE_ALLOCATE_BUFFER = 0x00000100;
      public static final int FORMAT_MESSAGE_FROM_SYSTEM     = 0x00001000;

      final Kernel32 INSTANCE =
        (Kernel32) Native.loadLibrary("kernel32", Kernel32.class);
    }

    /**
     * A structure for holding information about physical and virtual memory.
     *
     * See {@link
     * http://msdn.microsoft.com/en-us/library/aa366770(VS.85).aspx} for
     * further details about this structure.
     */
    public static final class MEMORYSTATUSEX extends Structure {
      public int dwLength = size();
      public int dwMemoryLoad;
      public long ullTotalPhys;
      public long ullAvailPhys;
      public long ullTotalPageFile;
      public long ullAvailPageFile;
      public long ullTotalVirtual;
      public long ullAvailVirtual;
      public long ullAvailExtendedVirtual;
    }

    public long getPhysicalMemory() {
      // The Windows Kernel32 call GlobalMemoryStatusEx() fills a
      // MEMORYSTATUSEX structure with various facts about memory usage.
      final MEMORYSTATUSEX mstat = new MEMORYSTATUSEX();
      if (Kernel32.INSTANCE.GlobalMemoryStatusEx(mstat)) {
        mstat.read();
        return mstat.ullTotalPhys;
      }
      else {
        // GlobalMemoryStatusEx failed
        final String[] lpBuffer = new String[1];
        final int errno = Native.getLastError();
        final int msglen = Kernel32.INSTANCE.FormatMessage(
          Kernel32.FORMAT_MESSAGE_ALLOCATE_BUFFER |
            Kernel32.FORMAT_MESSAGE_FROM_SYSTEM,
          Pointer.NULL,
          errno,
          0,
          lpBuffer,
          0,
          Pointer.NULL
        ); 

        System.err.print(
          "Error " + errno + ": " + (msglen > 0 ? lpBuffer[0] : "no message"));

        return -1L;
      } 
    }
  }
}
