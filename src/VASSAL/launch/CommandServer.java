/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.io.IOUtils;

/**
 * The base class for socket servers for communication between the
 * {@link ModuleManager} and its children {@link Player} and {@link Editor}
 * processes. Concrete extensions will implement {@link #execute(Object)} to
 * process incoming requests.
 *
 * @see CommandClient
 * @author Joel Uckelman
 * @since 3.1.0
 * @deprecated Use {@link SignalDispatcher} instead.
 */
@Deprecated
public class CommandServer implements Runnable {
  private final ServerSocket serverSocket;

  public CommandServer(ServerSocket serverSocket) {
    this.serverSocket = serverSocket;
  }

  /**
   * Initialize the {@link Command}. Subclasses handling commands
   * which need access to some local state will override this method.
   */
  public void init(Command command) {}

  public void run() {
    Socket clientSocket = null;
    ObjectOutputStream out = null;
    ObjectInputStream in = null;
    try {
      clientSocket = serverSocket.accept();

      out = new ObjectOutputStream(clientSocket.getOutputStream());
      in = new ObjectInputStream(clientSocket.getInputStream());

      Object obj;
      Object result;
      try {
        while ((obj = in.readObject()) != null) {
          // Execute commands as they come and send back the reply
          if (obj instanceof Command) {
            final Command command = (Command) obj;
            init(command);
            result = command.execute();
          }
          else {
            result = "UNRECOGNIZED_COMMAND";
          }

          out.writeObject(result);
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
      ErrorDialog.showDetails(
        e,
        ThrowableUtils.getStackTrace(e),
        "Error.socket_error"
      );
    }
    finally {
      IOUtils.closeQuietly(clientSocket);
      IOUtils.closeQuietly(serverSocket);
    }
  }
}
