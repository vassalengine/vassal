/*
 * $Id: ErrorDialog.java 4289 2008-10-20 20:26:48Z uckelman $
 *
 * Copyright (c) 2008 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class DialogUtils {
  private DialogUtils() {}

  private static final Set<Object> disabled =
    Collections.synchronizedSet(new HashSet<Object>());

  public static boolean isDisabled(Object key) {
    return disabled.contains(key);
  }

  public static synchronized boolean setDisabledIfNot(Object key) {
    final boolean result = isDisabled(key);
    if (!result) setDisabled(key, true);
    return result;
  }

  public static void setDisabled(Object key, boolean disable) {
    if (disable) disabled.add(key);
    else disabled.remove(key);
  }

  private static final ExecutorService ex = Executors.newSingleThreadExecutor();

  public static void enqueue(Runnable runnable) {
    ex.submit(runnable);
  }
}
