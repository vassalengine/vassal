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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.concurrent;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import VASSAL.tools.ErrorDialog;

/**
 * @author Joel Uckelman
 * @since 3.1
 */
public class FutureUtils {
  private FutureUtils() {}

  /**
   * Waits on a {@link Future} to complete.
   *
   * @param future the <code>Future</code> on which to wait
   */
  public static void wait(Future<?> future) {
    try {
      future.get();
    }
    catch (CancellationException e) {
      e.printStackTrace();
    }
    catch (ExecutionException e) {
      ErrorDialog.bug(e);
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
  }
}
