/*
 * Copyright (c) 2023 by Joel Uckelman
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

package org.vassalengine.agent;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import net.bytebuddy.asm.Advice;

public class Win32ShellFolderManager2Interceptor {
  private static Method getMethod() {
    Method m = null;
    try {
      final Class<?> sfc = Class.forName("sun.awt.shell.Win32ShellFolder2"); 
      m = sfc.getMethod("isSpecial");
      m.setAccessible(true);
      return m;
    }
    catch (ClassNotFoundException | NoSuchMethodException e) {
      throw new IllegalStateException(e);
    }
  }

  private static final Method mSpecial = getMethod();

  public static boolean isSpecial(Object o) {
    try {
      return Boolean.TRUE.equals(mSpecial.invoke(o));
    }
    catch (InvocationTargetException | IllegalAccessException e) {
      throw new IllegalStateException(e);
    }
  }

  @Advice.OnMethodExit()
  public static void exit(@Advice.Argument(0) Object a, @Advice.Argument(1) Object b, @Advice.Return(readOnly = false) int ret) {
    // The intercepted method mishandles the case where only one argument
    // is special. This fixes it up.
    final boolean special_a = isSpecial(a);
    final boolean special_b = isSpecial(b);
    if (special_a && !special_b) {
      ret = -1;
    }
    else if (!special_a && special_b) {
      ret = 1;
    }
  }
}
