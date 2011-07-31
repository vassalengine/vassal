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
import java.net.Socket;

import VASSAL.tools.ErrorDialog;

/**
 * A socket client for communication between the {@link ModuleManager}
 * and its children {@link Player} and {@link Editor} processes. Requests
 * are sent from, and replies recieved by this class.
 *
 * @see CommandServer
 * @author Joel Uckelman
 * @since 3.1.0
 * @deprecated Use {@link ObjectOutputStream} with {@link SerializableSingal}
 * instead.
 */
@Deprecated
public class CommandClient {
  private final ObjectInputStream in;
  private final ObjectOutputStream out;

  /**
   * Create a new <code>CommandClient</code>.
   *
   * @param clientSocket the socket on which to communicate
   * @throws IOException if something goes wrong with the socket
   */
  public CommandClient(Socket clientSocket) throws IOException {
    out = new ObjectOutputStream(clientSocket.getOutputStream());
    in = new ObjectInputStream(clientSocket.getInputStream());
  }

  /**
   * Send a request to the socket listener and recieve a reply.
   * This method is synchronized to ensure that only one thread
   * sends a requests over the socket at a time.
   *
   * @param cmd the command to send to the socket listener
   * @return the reply object from the socket listener
   * @throws IOException if something goes wrong with the socket
   */
  public synchronized Object request(Object cmd) throws IOException {
    out.writeObject(cmd);
    try {
      return in.readObject();
    }
    catch (ClassNotFoundException e) {
      ErrorDialog.bug(e);
      return null;
    }
  }
}
