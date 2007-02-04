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
/*
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: Jun 4, 2003
 */
package VASSAL.chat.node;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Properties;
import VASSAL.tools.ArgsParser;

/**
 * The server-side Main class
 */
public class Server extends Thread {
  private AsynchronousServerNode rootNode;
  private ServerSocket socket;

  public Server(AsynchronousServerNode rootNode, int port) throws IOException {
    this.rootNode = rootNode;
    socket = new ServerSocket(port);
    System.err.println("Started server on port " + port);
    start();
  }

  public void run() {
    int consecutiveFailures = 0;
    while (consecutiveFailures < 10) {
      try {
        Socket s = socket.accept();
        new PlayerNode(s, rootNode);
        consecutiveFailures = 0;
      }
      catch (Exception e) {
        e.printStackTrace();
        consecutiveFailures++;
      }
    }
    System.exit(1);
  }

  public static void main(String[] args) throws Exception {
    Properties p = new ArgsParser(args).getProperties();

    int port = Integer.parseInt(p.getProperty("port", "5050"));
    String reportURL = p.getProperty("URL", "http://www.vassalengine.org/util/");
    if ("null".equals(reportURL)) {
      reportURL = null;
    }
    if (!"true".equals(p.getProperty("test"))) {
      new Server(new AsynchronousServerNode(reportURL), port);
      new LockWatcher(1000L*60*30,1000L*60,port).start();
    }
    if (p.getProperty("test") != null) {
      Socket soc = new Socket("localHost", port);
      SocketHandler handler = new BufferedSocketHandler(soc, new SocketWatcher() {
        public void handleMessage(String msg) {
          System.err.println(msg);
        }

        public void socketClosed(SocketHandler handler) {
        }
      });
      handler.start();
      handler.writeLine(Protocol.encodeRegisterCommand("rk", "test/Main Room", ""));
      BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
      String line;
      while ((line = reader.readLine()) != null) {
        if (line.startsWith("JOIN")) {
          String room = line.substring("JOIN".length()).trim();
          handler.writeLine(Protocol.encodeJoinCommand("test/" + room));
        }
        else if (line.startsWith("BYE")) {
          handler.close();
        }
        else if (line.startsWith("HELLO")) {
          soc = new Socket("localHost", port);
          handler = new BufferedSocketHandler(soc, new SocketWatcher() {
            public void handleMessage(String msg) {
              System.err.println(msg);
            }

            public void socketClosed(SocketHandler handler) {
            }
          });
          handler.start();
          handler.writeLine(Protocol.encodeRegisterCommand("rk", "test/Main Room", ""));
        }
        else if (line.startsWith("*")) {
          int length = Integer.parseInt(line.substring(1));
          StringBuffer buffer = new StringBuffer();
          for (int i=0;i<length;++i) {
            char c = (char) ('a' + i%10);
            if (c == 'a') {
              c = 'A';
            }
            buffer.append(c);
          }
          String msg = Protocol.encodeForwardCommand("test/*",buffer.toString());
          handler.writeLine(msg);
        }
        else {
          handler.writeLine(line);
        }
      }
    }
  }
}
