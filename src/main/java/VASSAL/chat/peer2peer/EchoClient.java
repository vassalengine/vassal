/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jul 23, 2002
 * Time: 6:04:16 AM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.chat.peer2peer;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;
import java.util.Random;

import org.litesoft.p2pchat.PeerInfo;

import VASSAL.build.module.Chatter;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.SimpleRoom;
import VASSAL.command.Command;

// TODO: throw this away or make it a JUnit test
public class EchoClient implements Runnable, PropertyChangeListener {
  public static final String NAME = "EchoBot"; //$NON-NLS-1$
  private static Random rng = new Random();
  private int changeRoom;
  private int numRooms;
  private FileWriter log;
  private ChatServerConnection client;

  public EchoClient(ChatServerConnection client, int changeRoom, int numRooms, FileWriter log) {
    this.client = client;
    this.changeRoom = changeRoom;
    this.numRooms = numRooms;
    this.log = log;
    client.addPropertyChangeListener(ChatServerConnection.AVAILABLE_ROOMS, this);
    client.addPropertyChangeListener(ChatServerConnection.STATUS, new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        System.err.println(evt.getNewValue()); //$NON-NLS-1$
      }
    });
    client.setConnected(true);
    client.setRoom(new SimpleRoom("Room0")); //$NON-NLS-1$
    new Thread(this).start();
  }

  public synchronized void showCHAT(PeerInfo pPeerInfo, String msg) {
    Player sender = new P2PPlayer(pPeerInfo);
    if (!sender.getName().startsWith(NAME)
      && msg.startsWith("CHAT")) { //$NON-NLS-1$
      msg = "<" + client.getUserInfo().getName() //$NON-NLS-1$
        + msg.substring(msg.indexOf("> -")); //$NON-NLS-1$
      Command c = new Chatter.DisplayText(null,msg);
      client.sendTo(sender, c);
    }
  }

  @Override
  public void run() {
    while (true) {
      try {
        int nextSleep = Math.round(rng.nextFloat() * 2 * changeRoom * 1000);
        Thread.sleep(nextSleep);
      }
      catch (InterruptedException e) {
      }

      String newRoom = "Room" + (int) (numRooms * rng.nextFloat()); //$NON-NLS-1$
      client.setRoom(new SimpleRoom(newRoom));
    }
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    if (log != null
      && evt.getPropertyName().equals(ChatServerConnection.AVAILABLE_ROOMS)) {
      try {
        log.write("----------" + (new Date()) + "---------\n"); //$NON-NLS-1$ //$NON-NLS-2$
        log.write(report((VASSAL.chat.Room[]) evt.getNewValue()));
        log.flush();
      }
      // FIXME: review error message
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  public static String report(VASSAL.chat.Room[] r) {
    final StringBuilder buffer = new StringBuilder();
    for (VASSAL.chat.Room room : r) {
      buffer.append(room.getName() + ": "); //$NON-NLS-1$
      Player[] l = (Player[]) room.getPlayerList().toArray();
      for (int j = 0; j < l.length; ++j) {
        buffer.append(l[j]);
        if (j < l.length - 1) {
          buffer.append(", "); //$NON-NLS-1$
        }
      }
      buffer.append("\n"); //$NON-NLS-1$
    }
    return buffer.toString();
  }

/*  public static void main(String[] args) throws Exception {
    if (args.length == 0) {
      System.out.println("-module [module name] -clients [num clients] -host [host] -port [port] -rooms [num rooms] -wait [wait time] -log [true|false] -type [CGI|Proxy|Brokered|hierarchical]");
      System.exit(0);
    }
    Properties p = new ArgsParser(args).getProperties();
    String modName = p.getProperty("module","test");
    String host = p.getProperty("host","localhost");
    int port = Integer.parseInt(p.getProperty("port","5050"));
    int nClients = Integer.parseInt(p.getProperty("clients","2"));
    int nRooms = Integer.parseInt(p.getProperty("rooms","4"));
    int wait = Integer.parseInt(p.getProperty("wait","10"));
    boolean writeLogs = "true".equals(p.getProperty("log","true"));
    String poolType = p.getProperty("type","Proxy");
    StringTokenizer st = new StringTokenizer(modName, ",");
    MessageBoard msgSvr = new MessageBoard() {
      public Message[] getMessages() {
        return new Message[0];
      }

      public void postMessage(String msg) {
      }
    };
    WelcomeMessageServer welcomer = new WelcomeMessageServer() {
      public Command getWelcomeMessage() {
        return new NullCommand();
      }
    };
    while (st.hasMoreTokens()) {
      final String moduleName = st.nextToken();
      for (int i = 0; i < nClients; ++i) {
      final String userName = NAME+i;
        PeerPoolInfo info = new PeerPoolInfo() {
          public String getModuleName() {
            return moduleName;
          }

          public String getUserName() {
            return userName;
          }
        };
        PeerPool pool;
        pool = new DirectPeerPool();
        Thread.sleep((int) (wait * 1000 * rng.nextFloat()));
        ChatServerConnection client = null;
        if (poolType.startsWith("hier")) {
          client = new SocketNodeClient(new TextClient.Encoder(), info, host, port, msgSvr, welcomer);
        }
        else {
          client = new P2PClient(new TextClient.Encoder(), msgSvr, welcomer, pool);
        }
        client.setUserInfo(new SimplePlayer(userName));
        new EchoClient(client, wait, nRooms,
                       writeLogs ? new FileWriter("Log" + i) : null);
      }
    }
  }
*/
}
