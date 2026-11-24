/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.tools.filechooser;

import java.lang.invoke.MethodHandle;
import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Sends the standard Cocoa editing commands ({@code cut:}, {@code copy:},
 * {@code paste:}, {@code selectAll:}) to the text field of the open macOS
 * file dialog.
 *
 * <p>A text field in a Cocoa panel gets Cmd+X, Cmd+C, Cmd+V and Cmd+A only
 * from Edit menu items that send these commands to the first responder.
 * Java's screen menu bar has no way to make such items, so in VASSAL the
 * keys beep. {@link MacFileDialogMenus} supplies Swing items with those
 * shortcuts while a dialog is open, and their actions call this class.</p>
 *
 * <p>This uses the Foreign Function and Memory API, which is final in Java
 * 22. VASSAL is compiled for Java 17, which lacks it, so it is reached
 * through reflection; the Java bundled with VASSAL on macOS is new enough.
 * If the API is missing or anything fails, {@link #isAvailable()} is false,
 * no Edit items are offered, and the keys beep as before.</p>
 *
 * <p>The Objective-C runtime is called through {@code objc_msgSend}, with a
 * downcall handle for each argument shape used. The command itself is
 * delivered with {@code performSelectorOnMainThread:withObject:waitUntilDone:},
 * since AppKit objects must be used on the main thread and Swing actions
 * run on the event dispatch thread.</p>
 */
final class MacTextCommands {
  private static final Logger logger =
    LoggerFactory.getLogger(MacTextCommands.class);

  private static final String FFM = "java.lang.foreign."; //NON-NLS
  private static final String LIBOBJC = "/usr/lib/libobjc.A.dylib"; //NON-NLS

  private static MacTextCommands instance;
  private static boolean tried;

  private final Object arena;
  private final java.lang.reflect.Method allocateFrom;
  private final java.lang.reflect.Method address;
  private final Object nullPtr;

  private final MethodHandle getClass;    // id objc_getClass(const char *)
  private final MethodHandle registerSel; // SEL sel_registerName(const char *)
  private final MethodHandle sendPtr;     // id f(id, SEL)
  private final MethodHandle sendBool;    // BOOL f(id, SEL, id)
  private final MethodHandle sendPerform; // void f(id, SEL, SEL, id, BOOL)
  private final MethodHandle sendPtrArg;  // id f(id, SEL, id)
  private final MethodHandle sendVoidArg; // void f(id, SEL, id)
  private final MethodHandle sendLong;    // NSInteger f(id, SEL)
  private final MethodHandle sendAtIndex; // id f(id, SEL, NSInteger)

  private final Map<String, Object> selectors = new HashMap<>();

  private MacTextCommands() throws ReflectiveOperationException {
    final Class<?> linkerC = Class.forName(FFM + "Linker");
    final Class<?> arenaC = Class.forName(FFM + "Arena");
    final Class<?> lookupC = Class.forName(FFM + "SymbolLookup");
    final Class<?> segmentC = Class.forName(FFM + "MemorySegment");
    final Class<?> layoutC = Class.forName(FFM + "MemoryLayout");
    final Class<?> valueLayoutC = Class.forName(FFM + "ValueLayout");
    final Class<?> descriptorC = Class.forName(FFM + "FunctionDescriptor");
    final Class<?> allocatorC = Class.forName(FFM + "SegmentAllocator");
    final Class<?> optionC = Class.forName(FFM + "Linker$Option");

    final Class<?> layoutArrayC = Array.newInstance(layoutC, 0).getClass();
    final Object noOptions = Array.newInstance(optionC, 0);

    final Object linker = linkerC.getMethod("nativeLinker").invoke(null); //NON-NLS
    arena = arenaC.getMethod("global").invoke(null); //NON-NLS
    allocateFrom = allocatorC.getMethod("allocateFrom", String.class); //NON-NLS
    address = segmentC.getMethod("address"); //NON-NLS
    nullPtr = segmentC.getField("NULL").get(null); //NON-NLS

    final Object objc = lookupC.getMethod("libraryLookup", String.class, arenaC) //NON-NLS
      .invoke(null, LIBOBJC, arena);
    final java.lang.reflect.Method find = lookupC.getMethod("find", String.class); //NON-NLS

    final Object ptr = valueLayoutC.getField("ADDRESS").get(null); //NON-NLS
    final Object bool = valueLayoutC.getField("JAVA_BYTE").get(null); //NON-NLS
    final Object nsint = valueLayoutC.getField("JAVA_LONG").get(null); //NON-NLS

    final java.lang.reflect.Method of =
      descriptorC.getMethod("of", layoutC, layoutArrayC); //NON-NLS
    final java.lang.reflect.Method ofVoid =
      descriptorC.getMethod("ofVoid", layoutArrayC); //NON-NLS
    final java.lang.reflect.Method downcall =
      linkerC.getMethod("downcallHandle", segmentC, descriptorC, noOptions.getClass()); //NON-NLS

    final Object getClassFn = symbol(find, objc, "objc_getClass"); //NON-NLS
    final Object registerFn = symbol(find, objc, "sel_registerName"); //NON-NLS
    final Object msgSend = symbol(find, objc, "objc_msgSend"); //NON-NLS

    getClass = (MethodHandle) downcall.invoke(linker, getClassFn,
      of.invoke(null, ptr, layouts(layoutC, ptr)), noOptions);
    registerSel = (MethodHandle) downcall.invoke(linker, registerFn,
      of.invoke(null, ptr, layouts(layoutC, ptr)), noOptions);
    sendPtr = (MethodHandle) downcall.invoke(linker, msgSend,
      of.invoke(null, ptr, layouts(layoutC, ptr, ptr)), noOptions);
    sendBool = (MethodHandle) downcall.invoke(linker, msgSend,
      of.invoke(null, bool, layouts(layoutC, ptr, ptr, ptr)), noOptions);
    sendPerform = (MethodHandle) downcall.invoke(linker, msgSend,
      ofVoid.invoke(null, layouts(layoutC, ptr, ptr, ptr, ptr, bool)), noOptions);
    sendPtrArg = (MethodHandle) downcall.invoke(linker, msgSend,
      of.invoke(null, ptr, layouts(layoutC, ptr, ptr, ptr)), noOptions);
    sendVoidArg = (MethodHandle) downcall.invoke(linker, msgSend,
      ofVoid.invoke(null, layouts(layoutC, ptr, ptr, ptr)), noOptions);
    sendLong = (MethodHandle) downcall.invoke(linker, msgSend,
      of.invoke(null, nsint, layouts(layoutC, ptr, ptr)), noOptions);
    sendAtIndex = (MethodHandle) downcall.invoke(linker, msgSend,
      of.invoke(null, ptr, layouts(layoutC, ptr, ptr, nsint)), noOptions);
  }

  private static Object symbol(java.lang.reflect.Method find, Object lookup, String name)
      throws ReflectiveOperationException {
    final Optional<?> o = (Optional<?>) find.invoke(lookup, name);
    return o.orElseThrow(() -> new NoSuchMethodException(name));
  }

  private static Object layouts(Class<?> layoutC, Object... l) {
    final Object a = Array.newInstance(layoutC, l.length);
    for (int i = 0; i < l.length; ++i) {
      Array.set(a, i, l[i]);
    }
    return a;
  }

  /**
   * @return whether editing commands can be sent to a file dialog
   */
  static synchronized boolean isAvailable() {
    return get() != null;
  }

  private static synchronized MacTextCommands get() {
    if (!tried) {
      tried = true;
      try {
        instance = new MacTextCommands();
      }
      catch (ReflectiveOperationException | RuntimeException | LinkageError e) {
        // Older Java, or no native access: the dialog keeps its old manners
        logger.info("Cannot send editing commands to macOS file dialogs", e); //NON-NLS
      }
    }
    return instance;
  }

  /**
   * Sends an editing command, such as {@code "paste:"}, to the focused text
   * of the key window, if the key window is a file dialog.
   *
   * @param selector the Cocoa action, with its trailing colon
   */
  static synchronized void send(String selector) {
    final MacTextCommands m = get();
    if (m == null) {
      return;
    }

    try {
      m.sendToFileDialog(selector);
    }
    catch (Throwable t) { //NOPMD the method handles declare Throwable
      logger.warn("Could not send {} to the file dialog", selector, t); //NON-NLS
    }
  }

  /**
   * Makes the native items of a menu on the macOS menu bar send standard
   * Cocoa actions to the first responder, as the items of a native Edit menu
   * do, instead of calling back into Java. Cocoa then delivers the action
   * itself, on its own thread, to whatever has the focus in the key window.
   *
   * <p>The menu is found by the titles of its items, which must match
   * {@code titles} exactly and in order.</p>
   *
   * @param titles the titles of the menu's items
   * @param actions the Cocoa action for each item, such as {@code "paste:"}
   * @return whether the menu was found and rewired
   */
  static synchronized boolean wireToFirstResponder(String[] titles, String[] actions) {
    final MacTextCommands m = get();
    if (m == null) {
      return false;
    }

    try {
      return m.wire(titles, actions);
    }
    catch (Throwable t) { //NOPMD the method handles declare Throwable
      logger.warn("Could not wire the Edit menu to the file dialog", t); //NON-NLS
      return false;
    }
  }

  private boolean wire(String[] titles, String[] actions) throws Throwable {
    final Object app = sendPtr.invoke(cls("NSApplication"), sel("sharedApplication")); //NON-NLS
    final Object mainMenu = sendPtr.invoke(app, sel("mainMenu")); //NON-NLS
    if (isNull(mainMenu)) {
      return false;
    }

    final Object[] wanted = new Object[titles.length];
    for (int i = 0; i < titles.length; ++i) {
      wanted[i] = sendPtrArg.invoke(cls("NSString"), sel("stringWithUTF8String:"), cString(titles[i])); //NON-NLS
    }

    final long count = (Long) sendLong.invoke(mainMenu, sel("numberOfItems")); //NON-NLS
    for (long i = 0; i < count; ++i) {
      final Object top = sendAtIndex.invoke(mainMenu, sel("itemAtIndex:"), i); //NON-NLS
      final Object menu = sendPtr.invoke(top, sel("submenu")); //NON-NLS
      if (isNull(menu) ||
          (Long) sendLong.invoke(menu, sel("numberOfItems")) != titles.length) { //NON-NLS
        continue;
      }

      final Object[] items = new Object[titles.length];
      boolean match = true;
      for (int j = 0; match && j < titles.length; ++j) {
        items[j] = sendAtIndex.invoke(menu, sel("itemAtIndex:"), (long) j); //NON-NLS
        final Object title = sendPtr.invoke(items[j], sel("title")); //NON-NLS
        match = isTrue(sendBool.invoke(title, sel("isEqualToString:"), wanted[j])); //NON-NLS
      }

      if (match) {
        for (int j = 0; j < items.length; ++j) {
          // A nil target sends the action up the responder chain
          sendVoidArg.invoke(items[j], sel("setTarget:"), nullPtr); //NON-NLS
          sendVoidArg.invoke(items[j], sel("setAction:"), sel(actions[j])); //NON-NLS
        }
        return true;
      }
    }
    return false;
  }

  private void sendToFileDialog(String selector) throws Throwable {
    final Object app = sendPtr.invoke(cls("NSApplication"), sel("sharedApplication")); //NON-NLS
    final Object window = sendPtr.invoke(app, sel("keyWindow")); //NON-NLS
    if (isNull(window) ||
        !isTrue(sendBool.invoke(window, sel("isKindOfClass:"), cls("NSSavePanel")))) { //NON-NLS
      // NSOpenPanel is a kind of NSSavePanel. Anything else is not ours.
      return;
    }

    final Object responder = sendPtr.invoke(window, sel("firstResponder")); //NON-NLS
    final Object action = sel(selector);
    if (isNull(responder) ||
        !isTrue(sendBool.invoke(responder, sel("respondsToSelector:"), action))) { //NON-NLS
      return;
    }

    sendPerform.invoke(responder,
      sel("performSelectorOnMainThread:withObject:waitUntilDone:"), //NON-NLS
      action, nullPtr, (byte) 0);
  }

  private Object cls(String name) throws Throwable {
    return getClass.invoke(cString(name));
  }

  private Object sel(String name) throws Throwable {
    Object s = selectors.get(name);
    if (s == null) {
      s = registerSel.invoke(cString(name));
      selectors.put(name, s);
    }
    return s;
  }

  private Object cString(String s) throws ReflectiveOperationException {
    // The global arena never frees, so names are interned via the caches
    return allocateFrom.invoke(arena, s);
  }

  private boolean isNull(Object segment) throws ReflectiveOperationException {
    return (Long) address.invoke(segment) == 0L;
  }

  private static boolean isTrue(Object b) {
    return (Byte) b != 0;
  }
}
