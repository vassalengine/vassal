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

package VASSAL.launch;

import java.io.EOFException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import javax.swing.SwingUtilities;

import VASSAL.tools.CommunicationErrorDialog;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.io.IOUtils;

/**
 * The base class for socket servers for communication between the
 * {@link ModuleManager} and its children {@link Player} and {@link Editor}
 * processes. Concrete extensions will implement {@link #reply(Object)} to
 * process incoming requests. 
 *
 * @see CommandClient
 * @author Joel Uckelman
 * @since 3.1.0
 */
public abstract class CommandServer implements Runnable {
  private final ServerSocket serverSocket;

  public CommandServer(ServerSocket serverSocket) {
    this.serverSocket = serverSocket;
  }

  /**
   * Executes a command and returns a reply.
   *
   * Unlike the rest of <code>CommandServer</code>, this method will be run
   * on the EDT, so it is safe to call Swing methods from here.
   *
   * @param cmd a command object
   * @return the reply
   */
  protected abstract Object reply(Object cmd);

  private class Query implements Runnable {
    public Object cmd;
    public Object result;

    public void run() {
      result = reply(cmd);
    }
  }

  public void run() {
    Socket clientSocket = null;
    ObjectOutputStream out = null;
    ObjectInputStream in = null; 
    try {
      clientSocket = serverSocket.accept();

      out = new ObjectOutputStream(clientSocket.getOutputStream());
      in = new ObjectInputStream(clientSocket.getInputStream());
     
      final Query q = new Query();
      try {
        while ((q.cmd = in.readObject()) != null) {
          // Execute commands on the EDT
          try {
            SwingUtilities.invokeAndWait(q);
          }
          catch (InterruptedException e) {
            ErrorDialog.bug(e);
          }
          catch (InvocationTargetException e) {
            ErrorDialog.bug(e);
          }

          out.writeObject(q.result);
        }
      }
      catch (EOFException e) {
        // Normal. This happens when the socket is closed from the other end.
      }
      catch (SocketException e) {
        // Normal. This happens when the socket is closed from the other end.
      }

      clientSocket.close();
      serverSocket.close();
    }
    catch (ClassNotFoundException e) {
      ErrorDialog.bug(e);
    }
    catch (IOException e) {
      CommunicationErrorDialog.error(e);
    }
    finally {
      IOUtils.closeQuietly(clientSocket);
      IOUtils.closeQuietly(serverSocket);
    }
  }
}
