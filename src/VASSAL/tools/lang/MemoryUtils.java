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
package VASSAL.tools.lang;

import java.lang.management.ManagementFactory;

import org.apache.commons.lang.SystemUtils;

import VASSAL.tools.jna.Kernel32;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.PointerByReference;
import com.sun.management.OperatingSystemMXBean;

/**
 * A utility class for getting information about system memory.
 */
public class MemoryUtils {
  protected MemoryUtils() {}

  /**
   * Gets the amount of physical memory (RAM) in this machine, in bytes.
   *
   * @return the amount of RAM, in bytes; or -1 if the amount of RAM
   * cannot be queried.
   */
  public static long getPhysicalMemory() {
    if (!SystemUtils.IS_OS_WINDOWS ||
         SystemUtils.IS_OS_WINDOWS_98 || SystemUtils.IS_OS_WINDOWS_ME) {
      // Windows 98, ME support a maximum of 2GB RAM, so are unaffected
      // by the bug which causes incorrect reporting over 2GB. Hence,
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
