package VASSAL.chat.peer2peer;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Properties;

import org.litesoft.p2pchat.ActivePeerManager;
import org.litesoft.p2pchat.MyInfo;
import org.litesoft.p2pchat.PeerInfo;
import org.litesoft.p2pchat.PeerReader;
import org.litesoft.p2pchat.PeerWriter;
import org.litesoft.p2pchat.PendingPeerManager;
import org.litesoft.p2pchat.UserDialog;

public class UnitTest implements UserDialog {
  private String id;

  public UnitTest(String id) {
    this.id = id;
  }

  // TODO: move unit test to src/test/java, make this a JUnit test
  public static void main(String[] args) throws Exception {
    final Properties p = new Properties();

    if ("true".equals(p.getProperty("reader"))) { //$NON-NLS-1$ //$NON-NLS-2$
      //testReadWrite();
      testPeer();
    }

    if ("true".equals(p.getProperty("manager"))) { //$NON-NLS-1$ //$NON-NLS-2$
      testPeerManager();
    }

    if ("true".equals(p.getProperty("managers"))) { //$NON-NLS-1$ //$NON-NLS-2$
      testPeerManagers();
    }
  }

  private static void testPeerManager() throws InterruptedException,
                                               IOException {
    startSocketReader(5555);
    final UserDialog d = new UnitTest("Client"); //$NON-NLS-1$
    final PendingPeerManager ppm = new PendingPeerManager(d);
    final ActivePeerManager apm =
      new ActivePeerManager(new MyInfo("TestClient",5556),d,ppm); //$NON-NLS-1$
    final PeerInfo info =
      new PeerInfo("SocketReader","localhost",5555); //$NON-NLS-1$ //$NON-NLS-2$
    info.setID("SocketReader"); //$NON-NLS-1$
    ppm.addNewPeer(info);
    Thread.sleep(10000);
    apm.sendToAllCHAT("A message"); //$NON-NLS-1$
    apm.getPeerListenerByID("1").close(); //$NON-NLS-1$
//    apm.clear();
    System.err.println("Closed peer"); //$NON-NLS-1$
  }

  private static void testPeerManagers() throws InterruptedException, IOException {
    UserDialog serverDialog = new UnitTest("Server"); //$NON-NLS-1$
    PendingPeerManager serverPpm = new PendingPeerManager(serverDialog);
    new AcceptPeerThread(5555,serverPpm).start();

    UserDialog d = new UnitTest("Client"); //$NON-NLS-1$
    PendingPeerManager ppm = new PendingPeerManager(d);
    ActivePeerManager apm = new ActivePeerManager(new MyInfo("Client",5556),d,ppm); //$NON-NLS-1$
    PeerInfo info = new PeerInfo("Server","localhost",5555); //$NON-NLS-1$ //$NON-NLS-2$
    info.setID("Server"); //$NON-NLS-1$
    ppm.addNewPeer(info);
    Thread.sleep(5000);
    apm.sendToAllCHAT("A message"); //$NON-NLS-1$
    apm.clear();
  }

  private static void testPeer() throws InterruptedException, IOException {
    startSocketReader(5555);
/*
    UserDialog d = new UnitTest("Client");
    PendingPeerManager ppm = new PendingPeerManager(d);
    MyInfo myInfo = new MyInfo("TestClient",5556);
    ActivePeerManager apm = new ActivePeerManager(myInfo,d,ppm);
    PeerInfo info = new PeerInfo("Server","localhost",5555);
*/
    final Socket s = new Socket("localhost",5555); //$NON-NLS-1$

    final PeerReader reader = new PeerReader(s.getInputStream());
    System.err.println("Created reader " + reader); //$NON-NLS-1$
    try {
      final PeerWriter writer = new PeerWriter(s.getOutputStream());
      System.err.println("Created writer"+writer); //$NON-NLS-1$
//      ActivePeer peer = new ActivePeer(myInfo,d, apm, ppm, info, reader, writer);
//      peer.sendCHAT("A message");

      try {
        writer.writeLine("A message"); //$NON-NLS-1$
        Thread.sleep(2000);
      }
      finally {
        writer.close();
      }
    }
    finally {
      reader.close();
    }
  }

  private static void startSocketReader(final int port)
                                                  throws InterruptedException {
    final Object lock = new Object();

    final Runnable runnable = new Runnable() {
      @Override
      public void run() {
        try {
          ServerSocket server;
          synchronized (lock) {
            server = new ServerSocket(port);
            lock.notifyAll();
          }

          final Socket s = server.accept();
          PeerReader reader = null;
          try {
            reader = new PeerReader(s.getInputStream());
            while (true) {
              final String msg = reader.readLine();
              System.err.println(msg == null ? "" : msg); //$NON-NLS-1$
              if (msg == null) {
                break;
              }
            }
            System.err.println("Done"); //$NON-NLS-1$
            reader.close();
          }
          finally {
            if (reader != null) reader.close();
          }
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
    };

    synchronized (lock) {
      new Thread(runnable).start();
      lock.wait();
    }
  }

  @Override
  public void setActivePeerManager(ActivePeerManager pActivePeerManager) {
  }

  @Override
  public void setPendingPeerManager(PendingPeerManager pPendingPeerManager) {
  }

  @Override
  public void showUnrecognized(PeerInfo pPeerInfo, String pBadMessage) {
    System.err.println(this+"Unrecognized "+pPeerInfo); //$NON-NLS-1$
  }

  @Override
  public void showStreamsFailed(PeerInfo pPeerInfo) {
    System.err.println(this+"Streams Failed "+pPeerInfo); //$NON-NLS-1$
  }

  @Override
  public void showConnectFailed(PeerInfo pPeerInfo) {
    System.err.println(this+"Connect Failed "+pPeerInfo); //$NON-NLS-1$
  }

  @Override
  public void showConnect(PeerInfo pPeerInfo) {
    System.err.println(this+"Connect "+pPeerInfo); //$NON-NLS-1$
  }

  @Override
  public void showDisconnect(PeerInfo pPeerInfo) {
    System.err.println(this+"Disconnect "+pPeerInfo); //$NON-NLS-1$
  }

  @Override
  public void showCHAT(PeerInfo pPeerInfo, String pMessage) {
    System.err.println(this+"Chat "+pPeerInfo); //$NON-NLS-1$
  }

  @Override
  public void showPMSG(PeerInfo pPeerInfo, String pMessage) {
    System.err.println(this+"Private Chat "+pPeerInfo); //$NON-NLS-1$
  }

  @Override
  public void showNAME(PeerInfo pPeerInfo) {
    System.err.println(this+"Name "+pPeerInfo); //$NON-NLS-1$
  }

  @Override
  public void showHELO(PeerInfo pPeerInfo) {
    System.err.println(this+"Hello "+pPeerInfo); //$NON-NLS-1$
  }

  public String toString() {
    return id+": "; //$NON-NLS-1$
  }
}
