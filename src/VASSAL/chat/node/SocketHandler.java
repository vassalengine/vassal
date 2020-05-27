/*
 * $Id$
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class SocketHandler {
  private Socket sock;
  private SocketWatcher handler;
  private BufferedReader reader;
  private BufferedWriter writer;
  private BlockingQueue<String> writeQueue = new LinkedBlockingQueue<>();
  private boolean isOpen = true;
  private Thread readThread;
  private Thread writeThread;

  private static final String SIGN_OFF = "!BYE"; //$NON-NLS-1$

  public SocketHandler(Socket sock, SocketWatcher handler) throws IOException {
    this.sock = sock;
    this.handler = handler;
    reader = new BufferedReader(new InputStreamReader(sock.getInputStream(), StandardCharsets.UTF_8));
    writer = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream(), StandardCharsets.UTF_8));
  }

  public void start() {
    if (readThread == null) {
      readThread = startReadThread();
    }
    if (writeThread == null) {
      writeThread = startWriteThread();
    }
  }

  private Thread startReadThread() {
    final Runnable r = new Runnable() {
      @Override
      public void run() {
        String line;
        try {
          while ((line = readNext()) != null) {
            if (SIGN_OFF.equals(line)) {
              break;
            }
            else if (line.length() > 0) {
              try {
                handler.handleMessage(line);
              }
              // FIXME: review error message
              catch (Exception e) {
                // Handler threw an exception.  Keep reading.
                System.err.println("Caught " + e.getClass().getName() + " handling " + line); //$NON-NLS-1$ //$NON-NLS-2$
                e.printStackTrace();
              }
            }
          }
        }
        // FIXME: review error message
        catch (IOException ignore) {
          String msg = ignore.getClass().getName();
          msg = msg.substring(msg.lastIndexOf('.') + 1);
//          System.err.println("Caught " + msg + "(" + ignore.getMessage() + ") reading socket.");
        }
        closeSocket();
      }
    };

    final Thread t = new Thread(r, "read " + sock.getInetAddress());
    t.start();
    return t;
  }

  private Thread startWriteThread() {
    final Runnable r = new Runnable() {
      @Override
      public void run() {
        String line;
        try {
          while (true) {
            if ((line = getLine()) != null) {
              writeNext(line);
              if (SIGN_OFF.equals(line)) break;
            }
          }
        }
        // FIXME: review error message
        catch (IOException ignore) {
          String msg = ignore.getClass().getName();
          msg = msg.substring(msg.lastIndexOf('.') + 1);
//          System.err.println("Caught " + msg + "(" + ignore.getMessage() + ") writing to socket.");
        }
        closeSocket();
      }
    };

    final Thread t = new Thread(r, "write " + sock.getInetAddress());
    t.start();
    return t;
  }

  private String readNext() throws IOException {
    return reader.readLine();
  }

  private void writeNext(String line) throws IOException {
    writer.write(line + '\n');
    writer.flush();
  }

  public void writeLine(String pMessage) {
    try {
      writeQueue.put(pMessage);
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  private String getLine() {
    try {
      return writeQueue.take();
    }
    catch (InterruptedException e) {
      e.printStackTrace();
      return null;
    }
  }

  public void close() {
    writeLine(SIGN_OFF);
  }

  private void closeStreams() throws IOException {
    writer.close();
    reader.close();
  }

  private synchronized void closeSocket() {
    if (isOpen) {
      try {
        closeStreams();
      }
      // FIXME: review error message
      catch (IOException ignore) {
      }
      try {
        sock.close();
      }
      // FIXME: review error message
      catch (IOException ignore) {
      }

      close();
      isOpen = false;
      handler.socketClosed(this);
    }
  }

  public InetAddress getInetAddress() {
    return sock.getInetAddress();
  }
}
