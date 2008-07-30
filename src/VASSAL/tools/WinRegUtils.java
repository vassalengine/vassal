
package VASSAL.tools;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import VASSAL.Info;

public class WinRegUtils {

  public static final int HKEY_CLASSES_ROOT  = 0x80000000;
  public static final int HKEY_CURRENT_USER  = 0x80000001;
  public static final int HKEY_LOCAL_MACHINE = 0x80000002;
  
  public static final int ERROR_SUCCESS = 0;
  public static final int ERROR_FILE_NOT_FOUND = 2;
  public static final int ERROR_ACCESS_DENIED = 5;

  public static final int NATIVE_HANDLE = 0;
  public static final int ERROR_CODE = 1;
  public static final int SUBKEYS_NUMBER = 0;
  public static final int VALUES_NUMBER = 2;
  public static final int MAX_KEY_LENGTH = 3;
  public static final int MAX_VALUE_NAME_LENGTH = 4;

  // security masks for openKey
  public static final int DELETE = 0x10000;
  public static final int KEY_QUERY_VALUE = 1;
  public static final int KEY_SET_VALUE = 2;
  public static final int KEY_CREATE_SUB_KEY = 4;
  public static final int KEY_ENUMERATE_SUB_KEYS = 8;
  public static final int KEY_READ = 0x20019;
  public static final int KEY_WRITE = 0x20006;
  public static final int KEY_ALL_ACCESS = 0xF003F;
 
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
    final byte[] ret = invoker(instance.windowsRegQueryValueEx,
                               byte[].class, hKey, toByteArray(valueName));
    return ret == null ? null : new String(ret).trim();
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
    final byte[] ret = invoker(instance.windowsRegEnumKeyEx,
                               byte[].class, hKey, subKeyIndex, maxKeyLength);
    return ret == null ? null : new String(ret).trim();
  }

  public static String enumValue(int hKey, int valueIndex,
                                 int maxValueNameLength)
                                                      throws RegistryException{
    final byte[] ret =
      invoker(instance.windowsRegEnumValue,
              byte[].class, hKey, valueIndex, maxValueNameLength);
    return ret == null ? null : new String(ret).trim();
  }

  private static byte[] toByteArray(String s) {
    final byte[] tmp = s.getBytes();
    final byte[] bytes = new byte[tmp.length+1];
    System.arraycopy(tmp, 0, bytes, 0, tmp.length);
    bytes[tmp.length] = 0;
    return bytes;
  }

  public static class RegistryException extends Exception {
    private static final long serialVersionUID = 1L;

    public RegistryException(Throwable t) {
      super(t);
    }
  }

  public static void main(String[] args) throws RegistryException {
    final String jreRoot = "SOFTWARE\\JavaSoft\\Java Runtime Environment";
    final String jdkRoot = "SOFTWARE\\JavaSoft\\Java Development Kit";

    final String currentVersion = "CurrentVersion";
    final String javaHome = "JavaHome";

    String path = null;

    int handle;
    int[] ret;

    ret = WinRegUtils.openKey(
      WinRegUtils.HKEY_LOCAL_MACHINE,
      jreRoot,
      WinRegUtils.KEY_READ
    );

    if (ret[WinRegUtils.ERROR_CODE] == WinRegUtils.ERROR_SUCCESS) {
      handle = ret[WinRegUtils.NATIVE_HANDLE];

      final String curVer = WinRegUtils.queryValueEx(handle, currentVersion);
      WinRegUtils.closeKey(handle);

      if (curVer != null) {
        ret = WinRegUtils.openKey(
          WinRegUtils.HKEY_LOCAL_MACHINE,
          jreRoot + "\\" + curVer,
          WinRegUtils.KEY_READ);

        if (ret[WinRegUtils.ERROR_CODE] == WinRegUtils.ERROR_SUCCESS) {
          handle = ret[WinRegUtils.NATIVE_HANDLE];
          path = WinRegUtils.queryValueEx(handle, javaHome);
          WinRegUtils.closeKey(handle);
        }
      }
    }

    if (path == null) {
      ret = WinRegUtils.openKey(
        WinRegUtils.HKEY_LOCAL_MACHINE,
        jdkRoot,
        WinRegUtils.KEY_READ
      );

      if (ret[WinRegUtils.ERROR_CODE] == WinRegUtils.ERROR_SUCCESS) {
        handle = ret[WinRegUtils.NATIVE_HANDLE];

        final String curVer = WinRegUtils.queryValueEx(handle, currentVersion);
        WinRegUtils.closeKey(handle);

        if (curVer != null) {
          ret = WinRegUtils.openKey(
            WinRegUtils.HKEY_LOCAL_MACHINE,
            jdkRoot + "\\" + curVer,
            WinRegUtils.KEY_READ);

          if (ret[WinRegUtils.ERROR_CODE] == WinRegUtils.ERROR_SUCCESS) {
            handle = ret[WinRegUtils.NATIVE_HANDLE];
            path = WinRegUtils.queryValueEx(handle, javaHome);
            WinRegUtils.closeKey(handle);
          }
        }
      }
    }

    System.out.println(path);
  }
}
