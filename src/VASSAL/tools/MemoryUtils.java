/*
 * $Id$
 *
 * Copyright (c) 2009, 2010 by Joel Uckelman 
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

import java.lang.management.ManagementFactory;
import java.util.HashMap;
import java.util.Map;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIFunctionMapper;
import com.sun.jna.win32.W32APITypeMapper;

import com.sun.management.OperatingSystemMXBean;

/**
 * A utility class for getting information about system memory.
 */
public class MemoryUtils {
  private MemoryUtils() {}

  /**
   * Gets the amount of physical memory (RAM) in this machine, in bytes.
   *
   * @return the amount of RAM, in bytes; or -1 if the amount of RAM
   * cannot be queried.
   */
  public static long getPhysicalMemory() {
    // FIXME: use org.apache.commons.lang.SystemUtils for OS check in 3.2
    final String os = System.getProperty("os.name").toLowerCase();

    if (!os.startsWith("windows") ||
          os.equals("windows 98") || os.equals("windows me")) {
      // Windows 98, ME support a maximum of 2GB RAM, so are unaffected
      // by the bug in which causes incorrect reporting over 2GB. Hence,
      // we can handle them in the normal way.

      final Object o = ManagementFactory.getOperatingSystemMXBean();

      if (o instanceof OperatingSystemMXBean) {
        final OperatingSystemMXBean osb = (OperatingSystemMXBean) o;
        return osb.getTotalPhysicalMemorySize();
      }
      else {
        // We didn't get a com.sun.management.OperatingSystemMXBean. This
        // can happen if we're running on a non-Sun JVM.
        return -1;
      }
    }
    else {
      // FIXME: totalPhysicalMemorySize() doesn't return the correct result
      // on Windows machines with more than 2GB RAM, so we have to call
      // GlobalMemoryStatusEx ourselves instead of letting the bean do it
      // for us. See Sun Bug 6853676. This case can be removed when the bug
      // is fixed in a released JVM.

      // The Windows Kernel32 call GlobalMemoryStatusEx() fills a
      // MEMORYSTATUSEX structure with various facts about memory usage.
      final Kernel32.MEMORYSTATUSEX mstat = new Kernel32.MEMORYSTATUSEX();
      if (Kernel32.INSTANCE.GlobalMemoryStatusEx(mstat)) {
        return mstat.ullTotalPhys;
      }
      else {
        // GlobalMemoryStatusEx failed
        final PointerByReference lpBuffer = new PointerByReference();
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

        final String message =
          msglen > 0 ? lpBuffer.getValue().getStringArray(0)[0] : "no message";

        throw new RuntimeException("Error " + errno + ": " + message);
      } 
    }
  }

  /**
   * The wrapper for Kernel32.dll.
   */
  private static interface Kernel32 extends StdCallLibrary {
    /**
     * A structure for holding information about physical and virtual memory.
     *
     * See {@link
     * http://msdn.microsoft.com/en-us/library/aa366770(VS.85).aspx} for
     * further details about this structure.
     */
    public static final class MEMORYSTATUSEX extends Structure {
      // Note: A Windows DWORDLONG is 64 bits, so we use a Java long here
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

    /**
     * Gets information about physical and virtual memory.
     *
     * See {@link http://msdn.microsoft.com/en-us/library/aa366589(VS.85).aspx}
     * for further details about this function.
     *
     * @param p the structure where values are returned
     * @return <code>true</code> on success
     */
    boolean GlobalMemoryStatusEx(MEMORYSTATUSEX p);
      
    /**
     * Translates system error codes to error messages.
     *
     * See {@link http://msdn.microsoft.com/en-us/library/ms679351(VS.85).aspx}
     * for further details about this function.
     */
    int FormatMessage(
      int dwFlags, Pointer lpSource, int dwMessageId, int dwLanguageId,
      PointerByReference lpBuffer, int nSize, Pointer va_list);

    public static final int FORMAT_MESSAGE_ALLOCATE_BUFFER = 0x00000100;
    public static final int FORMAT_MESSAGE_FROM_SYSTEM     = 0x00001000;

    static final Map<Object,Object> OPTIONS = new HashMap<Object,Object>() {
      private static final long serialVersionUID = 1L;
  
      {
        // tell Kernel32 to use Unicode
        put(OPTION_TYPE_MAPPER,     W32APITypeMapper.UNICODE);
        put(OPTION_FUNCTION_MAPPER, W32APIFunctionMapper.UNICODE);
      }
    };

    public static final Kernel32 INSTANCE =
      (Kernel32) Native.loadLibrary("kernel32", Kernel32.class, OPTIONS);
  }

  public static void main(String[] args) {
    final long ram = getPhysicalMemory();
    if (ram >= 0) {
      System.out.println("System reports " + (ram >> 20) + "MB RAM");
    }
    else {
      System.out.println("Could not determine amount of RAM");
    }
  }
}
