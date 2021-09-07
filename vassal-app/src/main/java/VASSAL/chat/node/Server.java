/*
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.charset.Charset;
import java.util.Properties;

import VASSAL.tools.ArgsParser;

/**
 * The server-side Main class
 */
public class Server extends Thread {
  private final AsynchronousServerNode rootNode;
  private final ServerSocket socket;

  public Server(AsynchronousServerNode rootNode, int port) throws IOException {
    this.rootNode = rootNode;
    socket = new ServerSocket(port);
    System.err.println("Started server on port " + port); //$NON-NLS-1$
    start();
  }

  @Override
  public void run() {
    int consecutiveFailures = 0;
    while (consecutiveFailures < 10) {
      try {
        final Socket s = socket.accept();
        new PlayerNode(s, rootNode);
        consecutiveFailures = 0;
      }
      // FIXME: review error message
      catch (final Exception e) {
        e.printStackTrace();
        consecutiveFailures++;
      }
    }
    System.err.println("Exiting due to consecutiveFailures");
    System.exit(1);
  }

  public static void main(String[] args) throws Exception {
    final Properties p = new ArgsParser(args).getProperties();

    final int port = Integer.parseInt(p.getProperty("port", "5050")); //$NON-NLS-1$ //$NON-NLS-2$
    String reportURL = p.getProperty("URL", "https://vassalengine.org/util/"); //$NON-NLS-1$ //$NON-NLS-2$
    if ("null".equals(reportURL)) { //$NON-NLS-1$
      reportURL = null;
    }
    if (!"true".equals(p.getProperty("test"))) { //$NON-NLS-1$ //$NON-NLS-2$
      new Server(new AsynchronousServerNode(reportURL), port);
      new LockWatcher(1000L * 60 * 30, 1000L * 60, port).start();
    }
    if (p.getProperty("test") != null) { //$NON-NLS-1$
      Socket soc = new Socket("localHost", port); //$NON-NLS-1$
      SocketHandler handler = new SocketHandler(soc, new SocketWatcher() {
        @Override
        public void handleMessage(String msg) {
          System.err.println(msg);
        }

        @Override
        public void socketClosed(SocketHandler handler) {
        }
      });
      handler.start();
      handler.writeLine(Protocol.encodeRegisterCommand("rk", "test/Main Room", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      // open stdin, use native encoding.
      final BufferedReader reader =
        new BufferedReader(new InputStreamReader(System.in, Charset.defaultCharset()));
      try {
        String line;
        while ((line = reader.readLine()) != null) {
          if (line.startsWith("JOIN")) { //$NON-NLS-1$
            final String room = line.substring("JOIN".length()).trim(); //$NON-NLS-1$
            handler.writeLine(Protocol.encodeJoinCommand("test/" + room)); //$NON-NLS-1$
          }
          else if (line.startsWith("BYE")) { //$NON-NLS-1$
            handler.close();
          }
          else if (line.startsWith("HELLO")) { //$NON-NLS-1$
            soc = new Socket("localHost", port); //$NON-NLS-1$
            handler = new SocketHandler(soc, new SocketWatcher() {
              @Override
              public void handleMessage(String msg) {
                System.err.println(msg);
              }

              @Override
              public void socketClosed(SocketHandler handler) {
              }
            });
            handler.start();
            handler.writeLine(Protocol.encodeRegisterCommand("rk", "test/Main Room", "")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
          }
          else if (line.startsWith("*")) { //$NON-NLS-1$
            final int length = Integer.parseInt(line.substring(1));
            final StringBuilder buffer = new StringBuilder();
            for (int i = 0; i < length; ++i) {
              char c = (char) ('a' + i % 10);
              if (c == 'a') {
                c = 'A';
              }
              buffer.append(c);
            }
            final String msg = Protocol.encodeForwardCommand("test/*", buffer.toString()); //$NON-NLS-1$
            handler.writeLine(msg);
          }
          else {
            handler.writeLine(line);
          }
        }
      }
      finally {
        try {
          reader.close();
        }
        // FIXME: review error message
        catch (final IOException e) {
          e.printStackTrace();
        }
      }
    }
  }
}
