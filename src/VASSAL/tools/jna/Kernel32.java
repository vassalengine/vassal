/*
 * $Id$
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
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

package VASSAL.tools.jna;

import java.util.HashMap;
import java.util.Map;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIFunctionMapper;
import com.sun.jna.win32.W32APITypeMapper;

/**
 * A wrapper for Windows' Kernel32.dll.
 *
 * @since 3.2.0
 * @author JoelUckelman
 */
public interface Kernel32 extends StdCallLibrary {
  /**
   * A structure for holding information about physical and virtual memory.
   *
   * See {@link http://msdn.microsoft.com/en-us/library/aa366770(VS.85).aspx}
   * for further details about this structure.
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
    PointerByReference lpBuffer, int nSize, Pointer va_list
  );

  public static final int FORMAT_MESSAGE_ALLOCATE_BUFFER = 0x00000100;
  public static final int FORMAT_MESSAGE_FROM_SYSTEM     = 0x00001000;

  static final Map<Object,Object> OPTIONS =
                                                 new HashMap<Object,Object>() {
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
