/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.chat.node;

import java.io.IOException;
import java.net.Socket;

/**
 * Watches for thread lock on a server.  Kills the runtime if unable to establish new connection
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: Jul 20, 2003
 */
public class LockWatcher extends Thread {
  private long delay;
  private long timeout;
  private int port;

  /**
   *
   * @param delay Time in milliseconds between connection attempts
   * @param timeout Wait time in milliseconds to establish a new connection before terminating
   */
  public LockWatcher(long delay, long timeout, int port) {
    this.delay = delay;
    this.timeout = timeout;
    this.port = port;
  }

  public void run() {
    while (true) {
      try {
        sleep(delay);
        pingServer();
      }
      catch (InterruptedException e) {
        break;
      }
    }
  }

  private void pingServer() {
    try {
      final Socket s = new Socket("localhost", port); //$NON-NLS-1$
      final Thread t = new Thread(new Timeout());
      SocketWatcher watcher = new SocketWatcher() {
        public void handleMessage(String msg) {
          t.interrupt();
        }

        public void socketClosed(SocketHandler handler) {
          System.err.println("Server closed socket"); //$NON-NLS-1$
        }
      };
      SocketHandler sender = new BufferedSocketHandler(s, watcher);
      sender.start();
      t.start();
      sender.writeLine(Protocol.encodeRegisterCommand("pinger", "ping/Main", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      try {
        t.join();
      }
      catch (InterruptedException e) {
      }

      sender.close();
    }
    // FIXME: review error message
    catch (IOException e) {
      e.printStackTrace();
    }
  }
  private class Timeout implements Runnable {
    public void run() {
      try {
        sleep(timeout);
        System.err.println("No response from server in "+(timeout/1000.0)+" seconds.  Terminating process"); //$NON-NLS-1$ //$NON-NLS-2$
        System.exit(0);
      }
      // FIXME: review error message
      catch (InterruptedException e) {
        System.err.println("Ping"); //$NON-NLS-1$
        // Interrupt means response received from server
      }
    }
  }
}
