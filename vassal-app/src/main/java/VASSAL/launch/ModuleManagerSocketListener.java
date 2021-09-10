/*
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman
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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;

public final class ModuleManagerSocketListener implements Runnable {
  private static final Logger logger =
    LoggerFactory.getLogger(ModuleManagerSocketListener.class);

  private final ServerSocket serverSocket;
  private final Function<Object, String> execute;

  public ModuleManagerSocketListener(ServerSocket serverSocket, Function<Object, String> execute) {
    this.serverSocket = serverSocket;
    this.execute = execute;
  }

  @SuppressWarnings("PMD.UseTryWithResources")
  @Override
  public void run() {
    try {
      Socket clientSocket = null;

      // TODO while can only complete by throwing, do not use exceptions for ordinary control flow
      while (true) {
        try {
          clientSocket = serverSocket.accept();
          final String message;

          try (ObjectInputStream in = new ObjectInputStream(
            new BufferedInputStream(clientSocket.getInputStream()))) {

            message = execute.apply(in.readObject());
          }
          clientSocket.close();

          if (message == null || clientSocket.isClosed()) continue;

          try (PrintStream out = new PrintStream(
            new BufferedOutputStream(clientSocket.getOutputStream()), true, StandardCharsets.UTF_8)) {
            out.println(message);
          }
        }
        catch (IOException e) {
          ErrorDialog.showDetails(
            e,
            ThrowableUtils.getStackTrace(e),
            "Error.socket_error"
          );
        }
        catch (ClassNotFoundException e) {
          ErrorDialog.bug(e);
        }
        finally {
          if (clientSocket != null) {
            try {
              clientSocket.close();
            }
            catch (IOException e) {
              logger.error("Error while closing client socket", e);
            }
          }
        }
      }
    }
    finally {
      if (serverSocket != null) {
        try {
          serverSocket.close();
        }
        catch (IOException e) {
          logger.error("Error while closing server socket", e);
        }
      }
    }
  }
}
