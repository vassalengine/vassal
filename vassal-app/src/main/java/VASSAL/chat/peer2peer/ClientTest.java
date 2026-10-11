package VASSAL.chat.peer2peer;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Random;

import VASSAL.chat.Player;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.WelcomeMessageServer;

// TODO: throw this away or make it a JUnit test
public class ClientTest extends P2PClient implements Runnable, PropertyChangeListener {
  private static Random rng = new Random();
  private final int changeRoom;
  private final int numRooms;
  private final FileWriter log;

  public ClientTest(PeerPool pool, WelcomeMessageServer welcomer, int changeRoom, int numRooms, FileWriter log) {
    super(new TextClient.Encoder(), welcomer, pool);
    this.changeRoom = changeRoom;
    this.numRooms = numRooms;
    this.log = log;
    addPropertyChangeListener(AVAILABLE_ROOMS, this);
    setConnected(true);
    new Thread(this).start();
  }

  @Override
  public void run() {
    while (true) {
      try {
        Thread.sleep(changeRoom * 1000);
      }
      catch (InterruptedException e) {
      }

      final String newRoom = "Room" + (int) (numRooms * rng.nextFloat()); //$NON-NLS-1$
      setRoom(new SimpleRoom(newRoom));
    }
  }

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
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

  public static String report(VASSAL.chat.Room[] r) {
    final StringBuilder buffer = new StringBuilder();
    for (final VASSAL.chat.Room room : r) {
      buffer.append(room.getName()).append(": "); //$NON-NLS-1$
      final List<Player> l = room.getPlayerList();
      for (int j = 0; j < l.size(); ++j) {
        buffer.append(l.get(j));
        if (j < l.size() - 1) {
          buffer.append(", "); //$NON-NLS-1$
        }
      }
      buffer.append('\n');
    }
    return buffer.toString();
  }

/*  public static void main(String[] args) throws Exception {
    if (args.length == 0) {
      System.err.println("Usage:  -module <module> -clients <clientCount> -rooms <roomCount> -wait <delay> -type <CGI|Proxy>");
      System.exit(0);
    }
    Properties p = new ArgsParser(args).getProperties();
    String modName = p.getProperty("module", "test");
    boolean useProxy = "CGI".equals(p.getProperty("type", "Proxy"));
    int nClients = Integer.parseInt(p.getProperty("clients", "2"));
    int nRooms = Integer.parseInt(p.getProperty("rooms", "4"));
    int wait = Integer.parseInt(p.getProperty("wait", "10"));
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
    for (int i = 0; i < nClients; ++i) {
      Thread.sleep((int) (wait * 1000 * rng.nextFloat()));
      final String moduleName = modName;
      final String userName = "client" + i;
      PeerPoolInfo info = new PeerPoolInfo() {
        public String getModuleName() {
          return moduleName;
        }

        public String getUserName() {
          return userName;
        }
      };
      PeerPool pool = new DirectPeerPool();
      new ClientTest(pool, welcomer, wait, nRooms, new FileWriter("Log" + i));
    }
  }
*/
}
