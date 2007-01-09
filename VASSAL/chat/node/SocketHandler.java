package VASSAL.chat.node;

import java.io.IOException;
import java.net.Socket;
import java.util.Vector;

/**
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: Aug 27, 2003
 */
public abstract class SocketHandler {
  protected Socket sock;
  protected SocketWatcher handler;
  private Vector writeQueue = new Vector();
  private boolean isOpen = true;
  private Thread readThread;
  private Thread writeThread;
  private static final String SIGN_OFF = "!BYE";

  public SocketHandler(Socket sock, SocketWatcher handler) throws IOException {
    this.sock = sock;
    this.handler = handler;
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
    Runnable runnable = new Runnable() {
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
              catch (Exception e) {
                // Handler threw an exception.  Keep reading.
                System.err.println("Caught " + e.getClass().getName() + " handling " + line);
                e.printStackTrace();
              }
            }
          }
        }
        catch (IOException ignore) {
          String msg = ignore.getClass().getName();
          msg = msg.substring(msg.lastIndexOf('.') + 1);
//          System.err.println("Caught " + msg + "(" + ignore.getMessage() + ") reading socket.");
        }
        closeSocket();
      }
    };
    Thread t = new Thread(runnable);
    t.start();
    return t;
  }

  private Thread startWriteThread() {
    Runnable runnable = new Runnable() {
      public void run() {
        String line;
        try {
          while (true) {
            line = getLine();
            if (line != null) {
              writeNext(line);
            }
            if (SIGN_OFF.equals(line)) {
              break;
            }
          }
        }
        catch (IOException ignore) {
          String msg = ignore.getClass().getName();
          msg = msg.substring(msg.lastIndexOf('.') + 1);
//          System.err.println("Caught " + msg + "(" + ignore.getMessage() + ") writing to socket.");
        }
        closeSocket();
      }
    };
    Thread t = new Thread(runnable);
    t.start();
    return t;
  }

  protected abstract void closeStreams() throws IOException ;

  protected abstract String readNext() throws IOException;

  protected abstract void writeNext(String line) throws IOException;

  public void writeLine(String pMessage) {
    synchronized (writeQueue) {
      writeQueue.addElement(pMessage);
      writeQueue.notifyAll();
    }
  }

  public void close() {
    writeLine(SIGN_OFF);
  }

  private synchronized void closeSocket() {
    if (isOpen) {
      try {
        closeStreams();
      }
      catch (IOException ignore) {
      }
      try {
        sock.close();
      }
      catch (IOException ignore) {
      }
      isOpen = false;
      handler.socketClosed(this);
    }
  }

  private String getLine() {
    synchronized (writeQueue) {
      if (writeQueue.size() == 0) {
        try {
          writeQueue.wait();
        }
        catch (InterruptedException e) {
        }
      }
      String message = "";
      if (writeQueue.size() > 0) {
        message = (String) writeQueue.elementAt(0);
        writeQueue.removeElementAt(0);
      }
      return message;
    }
  }
}
