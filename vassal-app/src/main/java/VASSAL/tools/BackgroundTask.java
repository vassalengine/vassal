/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import javax.swing.SwingUtilities;

/**
 * Utility task for starting a thread that performs one task,
 * {@link #doFirst}, then queues another another
 * task, {@link #doLater}, for the Event Handler thread to execute
 * This is basically a simple version of Sun's SwingWorker class.
 *
 * @deprecated Use {@link SwingWorker} now that we ship the JAR for it.
 */
@Deprecated
public abstract class BackgroundTask {
  public abstract void doFirst();

  public abstract void doLater();

  public Thread start() {
    final Runnable later = new Runnable() {
      @Override
      public void run() {
          doLater();
      }
    };
    Runnable first = new Runnable() {
      @Override
      public void run() {
        try {
            doFirst();
        }
        catch (Throwable t) {
            t.printStackTrace();
        }
        finally {
            SwingUtilities.invokeLater(later);
        }
      }
    };
    Thread t = new Thread(first);
    t.start();
    return t;
  }
}
