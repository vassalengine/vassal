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

  public static void main(String[] args) throws Exception {
    Properties p = new Properties();
    if ("true".equals(p.getProperty("reader"))) {
      //testReadWrite();
      testPeer();
    }
    if ("true".equals(p.getProperty("manager"))) {
      testPeerManager();
    }
    if ("true".equals(p.getProperty("managers"))) {
      testPeerManagers();
    }
  }

  private static void testPeerManager() throws InterruptedException, IOException {
    startSocketReader(5555);
    UserDialog d = new UnitTest("Client");
    PendingPeerManager ppm = new PendingPeerManager(d);
    ActivePeerManager apm = new ActivePeerManager(new MyInfo("TestClient",5556),d,ppm);
    PeerInfo info = new PeerInfo("SocketReader","localhost",5555);
    info.setID("SocketReader");
    ppm.addNewPeer(info);
    Thread.sleep(10000);
    apm.sendToAllCHAT("A message");
    apm.getPeerListenerByID("1").close();
//    apm.clear();
    System.err.println("Closed peer");
  }

  private static void testPeerManagers() throws InterruptedException, IOException {
    UserDialog serverDialog = new UnitTest("Server");
    PendingPeerManager serverPpm = new PendingPeerManager(serverDialog);
    new AcceptPeerThread(5555,serverPpm).start();

    UserDialog d = new UnitTest("Client");
    PendingPeerManager ppm = new PendingPeerManager(d);
    ActivePeerManager apm = new ActivePeerManager(new MyInfo("Client",5556),d,ppm);
    PeerInfo info = new PeerInfo("Server","localhost",5555);
    info.setID("Server");
    ppm.addNewPeer(info);
    Thread.sleep(5000);
    apm.sendToAllCHAT("A message");
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
    Socket s = new Socket("localhost",5555);
    PeerReader reader = new PeerReader(s.getInputStream());
    System.err.println("Created reader "+reader);
    PeerWriter writer = new PeerWriter(s.getOutputStream());
    System.err.println("Created writer"+writer);
//    ActivePeer peer = new ActivePeer(myInfo,d, apm, ppm, info, reader, writer);
//    peer.sendCHAT("A message");
    writer.writeLine("A message");
    Thread.sleep(2000);
    reader.close();
    writer.close();
  }

  private static void startSocketReader(final int port) throws InterruptedException {
    final Object lock = new Object();
    Runnable runnable = new Runnable() {
      public void run() {
        try {
          ServerSocket server;
          synchronized (lock) {
            server = new ServerSocket(port);
            lock.notifyAll();
          }
          Socket s = server.accept();
          PeerReader reader = new PeerReader(s.getInputStream());
          while (true) {
            String msg = reader.readLine();
            System.err.println("" + msg);
            if (msg == null) {
              break;
            }
          }
          System.err.println("Done");
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

  public void setActivePeerManager(ActivePeerManager pActivePeerManager) {
  }

  public void setPendingPeerManager(PendingPeerManager pPendingPeerManager) {
  }

  public void showUnrecognized(PeerInfo pPeerInfo, String pBadMessage) {
    System.err.println(this+"Unrecognized "+pPeerInfo);
  }

  public void showStreamsFailed(PeerInfo pPeerInfo) {
    System.err.println(this+"Streams Failed "+pPeerInfo);
  }

  public void showConnectFailed(PeerInfo pPeerInfo) {
    System.err.println(this+"Connect Failed "+pPeerInfo);
  }

  public void showConnect(PeerInfo pPeerInfo) {
    System.err.println(this+"Connect "+pPeerInfo);
  }

  public void showDisconnect(PeerInfo pPeerInfo) {
    System.err.println(this+"Disconnect "+pPeerInfo);
  }

  public void showCHAT(PeerInfo pPeerInfo, String pMessage) {
    System.err.println(this+"Chat "+pPeerInfo);
  }

  public void showPMSG(PeerInfo pPeerInfo, String pMessage) {
    System.err.println(this+"Private Chat "+pPeerInfo);
  }

  public void showNAME(PeerInfo pPeerInfo) {
    System.err.println(this+"Name "+pPeerInfo);
  }

  public void showHELO(PeerInfo pPeerInfo) {
    System.err.println(this+"Hello "+pPeerInfo);
  }

  public String toString() {
    return id+": ";
  }
}
