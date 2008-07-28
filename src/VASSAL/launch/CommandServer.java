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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;

import VASSAL.tools.CommunicationErrorDialog;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.IOUtils;

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

  protected abstract Object reply(Object cmd);

  public void run() {
    Socket clientSocket = null;
    ObjectOutputStream out = null;
    ObjectInputStream in = null; 
    try {
      clientSocket = serverSocket.accept();

      out = new ObjectOutputStream(clientSocket.getOutputStream());
      in = new ObjectInputStream(clientSocket.getInputStream());
     
      Object cmd;
      while ((cmd = in.readObject()) != null) {
        out.writeObject(reply(cmd));
      }

      out.close();
      in.close();
      clientSocket.close();
    }
    catch (ClassNotFoundException e) {
      ErrorDialog.bug(e);
    }
    catch (IOException e) {
      CommunicationErrorDialog.error(e);
    }
    finally {
      IOUtils.closeQuietly(in);
      IOUtils.closeQuietly(out);
      IOUtils.closeQuietly(clientSocket);
    }
  }
}
