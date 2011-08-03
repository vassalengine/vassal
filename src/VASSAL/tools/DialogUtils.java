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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools;

import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import javax.swing.SwingUtilities;

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

  public static boolean setDisabled(Object key, boolean disable) {
    // we synchronize here to make atomic getting the previous
    // value and (possibly) setting a new one
    synchronized (disabled) {
      final boolean wasDisabled = isDisabled(key);

      if (wasDisabled) {
        if (!disable) disabled.remove(key);
      }
      else {
        if (disable) disabled.add(key);
      }

      return wasDisabled;
    }
  }

  private static final ExecutorService ex = Executors.newSingleThreadExecutor();

  public static Future<?> enqueue(final Runnable runnable) {
    return ex.submit(new Runnable() {
      public void run() {
        try {
          SwingUtilities.invokeAndWait(runnable);
        }
        catch (InterruptedException e) {
          ErrorDialog.bug(e);
        }
        catch (InvocationTargetException e) {
          ErrorDialog.bug(e);
        }
      }
    });
  }
}
