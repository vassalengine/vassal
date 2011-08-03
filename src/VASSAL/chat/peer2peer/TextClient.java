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
import java.util.Date;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;

public class TextClient {
  private boolean reportRooms = true;
  private ChatServerConnection client;

  public TextClient(ChatServerConnection client) {
    this.client = client;
    client.addPropertyChangeListener(ChatServerConnection.AVAILABLE_ROOMS, new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        availableRoomsChanged(evt);
      }
    });
    client.addPropertyChangeListener(ChatServerConnection.INCOMING_MSG, new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        incomingMessageReceived(evt);
      }
    });
    client.addPropertyChangeListener(ChatServerConnection.STATUS, new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        statusReceived(evt);
      }
    });
    client.setConnected(true);
  }

  public ChatServerConnection getClient() {
    return client;
  }

  private void statusReceived(PropertyChangeEvent evt) {
    System.out.println(String.valueOf(evt.getNewValue())); //$NON-NLS-1$
  }

  private void incomingMessageReceived(PropertyChangeEvent evt) {
    String msg = (String) evt.getNewValue();
    if (msg.startsWith("CHAT")) { //$NON-NLS-1$
      System.out.println(msg.substring(4));
    }
  }

  private void availableRoomsChanged(PropertyChangeEvent evt) {
    if (reportRooms) {
      System.out.println("----------" + (new Date()) + "---------"); //$NON-NLS-1$ //$NON-NLS-2$
      System.out.print(report((Room[]) evt.getNewValue()));
    }
  }

  public static String report(Room[] r) {
    final StringBuilder buffer = new StringBuilder();
    for (int i = 0; i < r.length; ++i) {
      buffer.append(r[i].getName() + ": "); //$NON-NLS-1$

      Player[] p = (Player[]) r[i].getPlayerList().toArray();
      for (int j = 0; j < p.length; ++j) {
        buffer.append(p[j]);
        if (j < p.length - 1) {
          buffer.append(", "); //$NON-NLS-1$
        }
      }
      buffer.append("\n"); //$NON-NLS-1$
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
      clientConnection = new SocketNodeClient(encoder, info, host, port, msgSvr, welcomer);
    }
    else {
      PeerPool pool;
      if ("CGI".equals(poolType)) {
        pool = new CgiPeerPool(info, "http://www.vassalengine.org/util/");
      }
      else if ("Proxy".equals(poolType)) {
        pool = new ProxyPeerPool(info, host, port);
      }
      else {
        pool = new BrokeredPeerPool(info, host, port);
      }
      clientConnection = new P2PClient(encoder, msgSvr, welcomer, pool);
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
*/  public static class ShowText extends Command {
    private String msg;

    public ShowText(String msg) {
      this.msg = msg;
    }

    public String getMessage() {
      return msg;
    }

    protected void executeCommand() {
      System.out.println(msg);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }

  public static class Encoder implements CommandEncoder {
    public Command decode(String command) {
      Command c = null;
      if (command.startsWith("CHAT")) { //$NON-NLS-1$
        c = new ShowText(command.substring(4));
      }
      return c;
    }

    public String encode(Command c) {
      String s = null;
      if (c instanceof ShowText) {
        return "CHAT"+((ShowText)c).getMessage(); //$NON-NLS-1$
      }
      return s;
    }


  }
}

