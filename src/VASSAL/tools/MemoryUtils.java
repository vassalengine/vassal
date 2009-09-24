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

import java.util.HashMap;
import java.util.Map;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.NativeLong;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.ptr.LongByReference;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIFunctionMapper;
import com.sun.jna.win32.W32APITypeMapper;

/**
 * A utility class for getting information about system memory.
 */
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
      final String arch = System.getProperty("os.arch").toLowerCase();
      if (arch.equals("i386") || arch.equals("x86") || arch.equals("amd64")) {
        // Force libc to load. An exception can occur here if we have an
        // architecture mismatch (e.g., a 32-bit JVM and no 32-bit libc,
        // only a 64-bit libc, or vice versa).
        LinuxMemoryUtilsImpl.Libc libc = null;
        try {
          libc = LinuxMemoryUtilsImpl.Libc.INSTANCE;
        }
        catch (UnsatisfiedLinkError e) {
          // libc remains null in this case.
        }

        IMPL = libc == null ? new DummyMemoryUtilsImpl() :
                              new LinuxMemoryUtilsImpl();
      }
      else {
        // JNA does not support Linux on non-x86 architectures (e.g., PPC,
        // Sparc, or ARM), so we have to use the dummy implementaion there.
        IMPL = new DummyMemoryUtilsImpl();
      }
    }
    else if (os.startsWith("mac os x")) {
      // Force libc to load. An exception can occur here if JNA is unable
      // to load libjnidispatch.jnilib from the normal library load path
      // and also fails to load its own copy, which is packed in the JNA
      // JAR. This seems to happen only with Mac OS X 10.4 running on PPC.
      MacOSXMemoryUtilsImpl.Libc libc = null;
      try {
        libc = MacOSXMemoryUtilsImpl.Libc.INSTANCE;
      }
      catch (UnsatisfiedLinkError e) {
        // libc remains null in this case.
      } 

      IMPL = libc == null ? new DummyMemoryUtilsImpl() :
                            new MacOSXMemoryUtilsImpl();
    }
    else if (os.startsWith("windows")) {
      // kernel32 didn't exist until Windows 2000, so use the dummy
      // implementation for Windows 98 and Windows ME.
      if (os.equals("windows 98") || os.equals("windows me")) {
        IMPL = new DummyMemoryUtilsImpl();
      }
      else {
        IMPL = new WindowsMemoryUtilsImpl();
      }
    }
    else {
      IMPL = new DummyMemoryUtilsImpl();
    }
  }

  private static class DummyMemoryUtilsImpl implements MemoryUtilsImpl {
    public long getPhysicalMemory() { return -1L; }
  }

  private static class LinuxMemoryUtilsImpl implements MemoryUtilsImpl{
    public static interface Libc extends Library {

      /**
       * The Java peer of the sysinfo struct.
       *
       * See the man page for <code>sysinfo(2)</code> for details.
       */
      public static final class sysinfo extends Structure {
        public NativeLong uptime;
        public NativeLong[] loads = new NativeLong[3];
        public NativeLong totalram;
        public NativeLong freeram;
        public NativeLong sharedram;
        public NativeLong bufferram;
        public NativeLong totalswap;
        public NativeLong freeswap;
        public short procs;
        public NativeLong totalhigh;
        public NativeLong freehigh;
        public int mem_unit;

        // Note: On 64-bit systems, _f would point to the end of the struct.
        // JNA complains if we have a zero-length byte array, so we make
        // sure that the padding is always at least one byte long.
        public byte[] _f = new byte[
          Math.max(20-2*NativeLong.SIZE-Native.getNativeSize(int.class), 1)];
      }

      /**
       * Wrapper for sysinfo(2).
       */
      int sysinfo(sysinfo info);

      /**
       * Wrapper for strerror(3).
       */
      String strerror(int errno);

      public static final Libc INSTANCE =
        (Libc) Native.loadLibrary("c", Libc.class);
    }

    public long getPhysicalMemory() {
      final Libc.sysinfo info = new Libc.sysinfo();
      if (Libc.INSTANCE.sysinfo(info) == 0) {
        return info.totalram.longValue() * info.mem_unit;
      }
      else {
        final int errno = Native.getLastError();
        System.err.println(
          "Error " + errno + ": " + Libc.INSTANCE.strerror(errno));

        return -1L;
      }
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

    public long getPhysicalMemory() {
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

        System.err.print("Error " + errno + ": " + message);

        return -1L;
      } 
    }
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
