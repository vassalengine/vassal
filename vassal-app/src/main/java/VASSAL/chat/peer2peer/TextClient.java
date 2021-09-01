package VASSAL.chat.peer2peer;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Room;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;

import java.beans.PropertyChangeEvent;
import java.util.Date;
import java.util.Objects;
import java.util.stream.Collectors;

public class TextClient {
  private final ChatServerConnection client;

  public TextClient(ChatServerConnection client) {
    this.client = client;
    client.addPropertyChangeListener(ChatServerConnection.AVAILABLE_ROOMS, this::availableRoomsChanged);
    client.addPropertyChangeListener(ChatServerConnection.INCOMING_MSG, this::incomingMessageReceived);
    client.addPropertyChangeListener(ChatServerConnection.STATUS, this::statusReceived);
    client.setConnected(true);
  }

  public ChatServerConnection getClient() {
    return client;
  }

  private void statusReceived(PropertyChangeEvent evt) {
    System.out.println(evt.getNewValue()); //$NON-NLS-1$
  }

  private void incomingMessageReceived(PropertyChangeEvent evt) {
    final String msg = (String) evt.getNewValue();
    if (msg.startsWith("CHAT")) { //$NON-NLS-1$
      System.out.println(msg.substring(4));
    }
  }

  private void availableRoomsChanged(PropertyChangeEvent evt) {
    System.out.println("----------" + (new Date()) + "---------"); //$NON-NLS-1$ //$NON-NLS-2$
    System.out.print(report((Room[]) evt.getNewValue()));
  }

  public static String report(Room[] r) {
    final StringBuilder buffer = new StringBuilder();
    for (final Room room : r) {
      buffer
        .append(room.getName())
        .append(": ") //$NON-NLS-1$
        .append(
          room
            .getPlayerList()
            .stream()
            .map(Objects::toString)
            .collect(Collectors.joining(", ")) //$NON-NLS-1$
        )
        .append('\n'); //$NON-NLS-1$
    }
    return buffer.toString();
  }

/*  public static void main(String[] args) throws Exception {
    if (args.length == 0) {
      System.out.println("Usage:  -module <module> -name <username> -type <hier|CGI|Brokered|Proxy> -host <host> -port <port>");
      System.exit(0);
    }
    if (System.getProperty("stderr") != null) {
      System.setErr(new PrintStream(new FileOutputStream(System.getProperty("stderr"))));
    }
    Properties p = new ArgsParser(args).getProperties();
    String modName = p.getProperty("module", "test");
    String myName = p.getProperty("name", "rk");
    String poolType = p.getProperty("type", "Brokered");
    String host = p.getProperty("host", "localhost");
    int port = Integer.parseInt(p.getProperty("port", "5050"));
    final BufferedReader input = new BufferedReader(new InputStreamReader(System.in));
    final String moduleName = modName;
    final String userName = myName;
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
    ChatServerConnection clientConnection;
    PeerPoolInfo info = new PeerPoolInfo() {
      public String getModuleName() {
        return moduleName;
      }

      public String getUserName() {
        return userName;
      }
    };
    CommandEncoder encoder = new Encoder();
    if (poolType.startsWith("hier")) {
      clientConnection = new NodeClient(encoder, info, host, port, msgSvr, welcomer);
    }
    else {
      PeerPool pool;
      if ("CGI".equals(poolType)) {
        pool = new CgiPeerPool(info, "https://vassalengine.org/util/");
      }
      else if ("Proxy".equals(poolType)) {
        pool = new ProxyPeerPool(info, host, port);
      }
      else {
        pool = new BrokeredPeerPool(info, host, port);
      }
      clientConnection = new P2PClient(encoder, welcomer, pool);
      if (pool instanceof BrokeredPeerPool) {
        final BrokeredPeerPool reportStatus = (BrokeredPeerPool) pool;
        final ChatServerConnection c = clientConnection;
        PropertyChangeListener l = new PropertyChangeListener() {
                  public void propertyChange(PropertyChangeEvent evt) {
                    reportStatus.sendStatus(c, moduleName);
                  }
                };
        clientConnection.addPropertyChangeListener(ChatServerConnection.ROOM, l);
        clientConnection.addPropertyChangeListener(ChatServerConnection.PLAYER_INFO, l);
      }
    }
    final TextClient client = new TextClient(clientConnection);
    client.getClient().setUserInfo(new SimplePlayer(userName));
    Runnable r = new Runnable() {
      public void run() {
        String s;
        try {
          while ((s = input.readLine()) != null) {
            if (s.startsWith("-room")) {
              client.reportRooms = false;
            }
            else if (s.startsWith("+room")) {
              client.reportRooms = true;
            }
            else if (s.startsWith("JOIN")) {
              s = s.substring("JOIN".length()).trim();
              client.getClient().setRoom(new SimpleRoom(s));
            }
            else if (s.startsWith("BYE")) {
              client.getClient().setConnected(false);
            }
            else if (s.startsWith("HELLO")) {
              client.getClient().setConnected(true);
            }
            else if (s.startsWith("SHOUT")) {
              s = s.substring("SHOUT".length()).trim();
              s = "CHAT<" + client.getClient().getUserInfo().getName()
                                           + "> - " + s;
              client.shout(s);
            }
            else if (s.startsWith("!")) {
              try {
                int length = Integer.parseInt(s.substring(1));
                StringBuilder msg = new StringBuilder();
                msg.append("CHAT<" + client.getClient().getUserInfo().getName()
                                           + "> ("+length+" chars) -");
                for (int i=0;i<length;++i) {
                  msg.append((char)('0'+i%10));
                }
                Command c = new ShowText(msg.toString());
                client.getClient().sendToOthers(c);
                client.getClient().sendTo(client.getClient().getUserInfo(),c);
              }
              catch (NumberFormatException e) {
              }
            }
            else if (s.length() > 0) {
              String msg = " - <" + client.getClient().getUserInfo().getName()
                                           + "> - " + s;
              Command c = new ShowText(msg);
              client.getClient().sendToOthers(c);
              client.getClient().sendTo(client.getClient().getUserInfo(),c);
            }
          }
        }
        catch (IOException ex) {
          ex.printStackTrace();
        }
      }
    };
    new Thread(r).start();
  }
*/

  public static class ShowText extends Command {
    private final String msg;

    public ShowText(String msg) {
      this.msg = msg;
    }

    public String getMessage() {
      return msg;
    }

    @Override
    protected void executeCommand() {
      System.out.println(msg);
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }

  public static class Encoder implements CommandEncoder {

    private static final String SERIALIZATION_PREFIX = "CHAT"; //$NON-NLS-1$

    @Override
    public Command decode(String command) {
      if (!command.startsWith(SERIALIZATION_PREFIX)) {
        return null;
      }
      return new ShowText(command.substring(SERIALIZATION_PREFIX.length()));
    }

    @Override
    public String encode(Command c) {
      if (!(c instanceof ShowText)) {
        return null;
      }
      return SERIALIZATION_PREFIX + ((ShowText)c).getMessage();
    }


  }
}

