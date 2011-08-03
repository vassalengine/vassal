package VASSAL.chat.peer2peer;

import java.io.IOException;
import java.net.ServerSocket;

import org.litesoft.p2pchat.PendingPeerManager;

/**
 * Date: Mar 11, 2003
 */
public class AcceptPeerThread extends Thread {
  private boolean running = true;
  private ServerSocket socket;
  private PendingPeerManager ppm;
  private int port;
  private static final int MAX_ATTEMPTS = 10;

  public AcceptPeerThread(int initialPort, PendingPeerManager ppm) throws IOException {
    this.ppm = ppm;
    for (int i=0;i<MAX_ATTEMPTS;++i) {
      port = initialPort+i;
      try {
        socket = new ServerSocket(port);
        break;
      }
      // FIXME: review error message
      catch (Exception ex) {
        if (i == MAX_ATTEMPTS -1) {
          // FIXME: switch to IOException(Throwable) ctor in Java 1.6
          throw (IOException) new IOException().initCause(ex);
        }
      }
    }
  }

  public int getPort() {
    return port;
  }

  public AcceptPeerThread(ServerSocket socket, PendingPeerManager ppm) {
    this.socket = socket;
    this.ppm = ppm;
  }

  public synchronized void start() {
    running = true;
    super.start();
  }

  public void run() {
    while (running) {
      try {
        ppm.addNewPeer(socket.accept());
      }
      // FIXME: review error message
      catch (Exception ex) {
        halt();
      }
    }
  }

  public void halt() {
    interrupt();
    running = false;
    try {
      socket.close();
    }
    // FIXME: review error message
    catch (IOException e) {
      e.printStackTrace();
    }
  }
}
