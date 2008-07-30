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
package VASSAL.tools;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import VASSAL.Info;

/**
 * A utility class for manipulating the Windows Registry. This class uses
 * reflection to leverage existing methods in
 * {@link java.util.prefs.WindowsPreferences} for reading and writing the
 * Windows Registry. It's an ugly hack, but one abomination deserves another.
 * Think hard about whether you <i>really</i> need to use the Windows Registry.
 * For a description of the registry manipulation functions, see
 * {@link http://msdn.microsoft.com/en-us/library/ms724875(VS.85).aspx}.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */ 
public class WinRegUtils {
  // Handles to various registry hives
  public static final int HKEY_CLASSES_ROOT  = 0x80000000;
  public static final int HKEY_CURRENT_USER  = 0x80000001;
  public static final int HKEY_LOCAL_MACHINE = 0x80000002;

  // Windows error codes 
  public static final int ERROR_SUCCESS = 0;
  public static final int ERROR_FILE_NOT_FOUND = 2;
  public static final int ERROR_ACCESS_DENIED = 5;

  // Constants used to interpret int[] returns
  public static final int NATIVE_HANDLE = 0;
  public static final int ERROR_CODE = 1;
  public static final int SUBKEYS_NUMBER = 0;
  public static final int VALUES_NUMBER = 2;
  public static final int MAX_KEY_LENGTH = 3;
  public static final int MAX_VALUE_NAME_LENGTH = 4;
  public static final int DISPOSITION = 2;
  public static final int REG_CREATED_NEW_KEY = 1;
  public static final int REG_OPENED_EXISTING_KEY = 2;
  public static final int NULL_NATIVE_HANDLE = 0;

  // Windows security masks
  public static final int DELETE = 0x10000;
  public static final int KEY_QUERY_VALUE = 1;
  public static final int KEY_SET_VALUE = 2;
  public static final int KEY_CREATE_SUB_KEY = 4;
  public static final int KEY_ENUMERATE_SUB_KEYS = 8;
  public static final int KEY_READ = 0x20019;
  public static final int KEY_WRITE = 0x20006;
  public static final int KEY_ALL_ACCESS = 0xF003F;

  // Methods pulled from java.util.prefs.WindowsPreferences
  private Method windowsRegOpenKey;
  private Method windowsRegCloseKey;
  private Method windowsRegCreateKeyEx;
  private Method windowsRegDeleteKey;
  private Method windowsRegFlushKey;
  private Method windowsRegQueryValueEx;
  private Method windowsRegSetValueEx;
  private Method windowsRegDeleteValue;
  private Method windowsRegQueryInfoKey;
  private Method windowsRegEnumKeyEx;
  private Method windowsRegEnumValue;

  private static final WinRegUtils instance = new WinRegUtils(); 

  private WinRegUtils() {
    if (!Info.isWindows()) throw new UnsupportedOperationException();

    // set up the methods from WindowsPreferences
    try {
      final Class<?> c = Class.forName("java.util.prefs.WindowsPreferences");

      windowsRegOpenKey = c.getDeclaredMethod(
        "WindowsRegOpenKey", int.class, byte[].class, int.class);
      windowsRegOpenKey.setAccessible(true);

      windowsRegCloseKey = c.getDeclaredMethod("WindowsRegCloseKey", int.class);
      windowsRegCloseKey.setAccessible(true);
                              
      windowsRegCreateKeyEx = c.getDeclaredMethod(
        "WindowsRegCreateKeyEx", int.class, byte[].class);
      windowsRegCreateKeyEx.setAccessible(true);

      windowsRegDeleteKey = c.getDeclaredMethod(
        "WindowsRegDeleteKey", int.class, byte[].class);
      windowsRegDeleteKey.setAccessible(true);

      windowsRegFlushKey = c.getDeclaredMethod("WindowsRegFlushKey", int.class);
      windowsRegFlushKey.setAccessible(true);

      windowsRegQueryValueEx = c.getDeclaredMethod(
        "WindowsRegQueryValueEx", int.class, byte[].class);
      windowsRegQueryValueEx.setAccessible(true);

      windowsRegSetValueEx = c.getDeclaredMethod(
        "WindowsRegSetValueEx", int.class, byte[].class, byte[].class);
      windowsRegSetValueEx.setAccessible(true);

      windowsRegDeleteValue = c.getDeclaredMethod(
        "WindowsRegDeleteValue", int.class, byte[].class);
      windowsRegDeleteValue.setAccessible(true);

      windowsRegQueryInfoKey = c.getDeclaredMethod(
        "WindowsRegQueryInfoKey", int.class);
      windowsRegQueryInfoKey.setAccessible(true);

      windowsRegEnumKeyEx = c.getDeclaredMethod(
        "WindowsRegEnumKeyEx", int.class, int.class, int.class);
      windowsRegEnumKeyEx.setAccessible(true);

      windowsRegEnumValue = c.getDeclaredMethod(
        "WindowsRegEnumValue", int.class, int.class, int.class);
      windowsRegEnumValue.setAccessible(true);
    }
    catch (ClassNotFoundException e) {
      throw new UnsupportedOperationException(e);
    }
    catch (NoSuchMethodException e) {
      throw new UnsupportedOperationException(e);
    }
  }

  /**
   * Invokes a {@link Method} with the given arguments and returns its result.
   *
   * @param m the method to invoke
   * @param ret the return type of the method
   * @param args the arguments with which to invoke the method
   * @return the result of the invoking the method
   * @throws RegistryException if something goes wrong
   */
  private static <T> T invoker(Method m, Class<T> ret, Object... args)
                                                     throws RegistryException {
    try {
      return ret.cast(m.invoke(null, args));
    }
    catch (IllegalAccessException e) {
      throw new RegistryException(e);
    }
    catch (IllegalArgumentException e) {
      throw new RegistryException(e);
    }
    catch (InvocationTargetException e) {
      throw new RegistryException(e);
    }
  }

  
  public static int[] openKey(int hKey, String subKey, int securityMask)
                                                     throws RegistryException {
    return invoker(instance.windowsRegOpenKey,
                   int[].class, hKey, toByteArray(subKey), securityMask);
  }

  public static int closeKey(int hKey) throws RegistryException {
    return invoker(instance.windowsRegCloseKey, Integer.class, hKey);
  }


  public static int[] createKeyEx(int hKey, String subKey)
                                                     throws RegistryException {
    return invoker(instance.windowsRegCreateKeyEx,
                   int[].class, hKey, toByteArray(subKey));
  }

  
  public static int deleteKey(int hKey, String subKey)
                                                     throws RegistryException {
    return invoker(instance.windowsRegDeleteKey,
                   int.class, hKey, toByteArray(subKey));
  }

  public static int flushKey(int hKey) throws RegistryException {
    return invoker(instance.windowsRegFlushKey, Integer.class, hKey);
  }

  public static String queryValueEx(int hKey, String valueName)
                                                    throws RegistryException {
    return toString(invoker(instance.windowsRegQueryValueEx,
                            byte[].class, hKey, toByteArray(valueName)));
  }


  public static int setValueEx(int hKey, String valueName, String value)
                                                    throws RegistryException {
    return invoker(instance.windowsRegSetValueEx,
                   Integer.class,
                   hKey, toByteArray(valueName), toByteArray(value));
  }

  public static int deleteValue(int hKey, String valueName) 
                                                    throws RegistryException {
    return invoker(instance.windowsRegDeleteValue,
                   Integer.class, hKey, toByteArray(valueName));
  }

  public static int[] queryInfoKey(int hKey) throws RegistryException {
    return invoker(instance.windowsRegQueryInfoKey, int[].class, hKey);
  }

  public static String enumKeyEx(int hKey, int subKeyIndex, int maxKeyLength)
                                                    throws RegistryException {
    return toString(invoker(instance.windowsRegEnumKeyEx,
                            byte[].class, hKey, subKeyIndex, maxKeyLength));
  }

  public static String enumValue(int hKey, int valueIndex,
                                 int maxValueNameLength)
                                                      throws RegistryException{
    return toString(
      invoker(instance.windowsRegEnumValue,
              byte[].class, hKey, valueIndex, maxValueNameLength));
  }

  /**
   * Returns the path to Sun's <code>java.exe</code>. JRE installations
   * are checked first, and then JDK installations.
   *
   * @return the path to <code>java.exe</code>, or <code>null</code> if
   * none can be found
   */
  public static String getJavaPath() {
    final String jreRoot = "SOFTWARE\\JavaSoft\\Java Runtime Environment";
    final String jdkRoot = "SOFTWARE\\JavaSoft\\Java Development Kit";

    String path = getJavaHome(jreRoot);
    if (path != null) {
      path += "\\bin\\java";
    }
    else {
      path = getJavaHome(jdkRoot);
      if (path != null) {
        path += "\\jre\\bin\\java";
      }
    }

    return path;
  }

  private static String getJavaHome(String root) {
    String path = null;

    int handle;
    int[] ret;

    try {
      ret = openKey(HKEY_LOCAL_MACHINE, root, KEY_READ);
      if (ret[ERROR_CODE] == ERROR_SUCCESS) {
        handle = ret[NATIVE_HANDLE];

        final String curVer = queryValueEx(handle, "CurrentVersion");
        closeKey(handle);

        if (curVer != null) {
          ret = openKey(HKEY_LOCAL_MACHINE, root + "\\" + curVer, KEY_READ);

          if (ret[ERROR_CODE] == ERROR_SUCCESS) {
            handle = ret[NATIVE_HANDLE];
            path = queryValueEx(handle, "JavaHome");
            closeKey(handle);
          }
        }
      }
    }
    catch (RegistryException e) {
      ErrorLog.log(e);
    }

    return path;
  }

  /**
   * Converts <code>String</code>s to null-terminated byte arrays.
   * 
   * @param s the <code>String</code> to convert
   * @return the string as a null-terminated byte array
   */  
  private static byte[] toByteArray(String s) {
    final byte[] tmp = s.getBytes();
    final byte[] bytes = new byte[tmp.length+1];
    System.arraycopy(tmp, 0, bytes, 0, tmp.length);
    bytes[tmp.length] = 0;
    return bytes;
  }

  /**
   * Converts a null-terminated byte array to a <code>String</code>.
   *
   * @param bytes the null-terminated byte array to convert
   * @return the null-terminated byte array as a <code>String</code>
   */
  private static String toString(byte[] bytes) {
    if (bytes == null) return null;
    
    final String s = new String(bytes);
    return s.charAt(s.length() - 1) == '\0' ?
      s.substring(0, s.length() - 1) : s;
  }

  public static class RegistryException extends Exception {
    private static final long serialVersionUID = 1L;

    public RegistryException(Throwable t) {
      super(t);
    }
  }

  public static void main(String[] args) throws RegistryException {
    System.out.println(getJavaPath());
  }
}
