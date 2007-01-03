package VASSAL.chat.peer2peer;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import org.litesoft.p2pchat.ActivePeer;
import org.litesoft.p2pchat.ActivePeerManager;
import org.litesoft.p2pchat.MyInfo;
import org.litesoft.p2pchat.PeerInfo;
import org.litesoft.p2pchat.PendingPeerManager;
import org.litesoft.p2pchat.UserDialog;
import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.messageboard.Message;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;

public class P2PClient implements ChatServerConnection, UserDialog {
  private Player me;
  private PendingPeerManager ppm;
  protected ActivePeerManager peerMgr;
  private PeerPool pool;
  private MessageBoard msgSvr;
  private WelcomeMessageServer welcomeMessageServer;
  private RoomManager roomMgr;
  private RoomTracker tracker;
  // TODO filter commands in the Main Room
//  private MainRoomChecker filter;
  private PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  private CommandEncoder encoder;
  private boolean connected = false;
  private ServerStatus svrStatus;

  public P2PClient(CommandEncoder encoder, MessageBoard msgSvr, WelcomeMessageServer welcomeMessageServer, PeerPool pool) {
    this.encoder = encoder;
    this.msgSvr = msgSvr;
    this.welcomeMessageServer = welcomeMessageServer;
    this.pool = pool;
    ppm = new PendingPeerManager(this);
    roomMgr = new RoomManager();
    tracker = new RoomTracker();
//    filter = new MainRoomChecker();
    me = new SimplePlayer("???");
  }

  public RoomManager getRoomMgr() {
    return roomMgr;
  }

  public void sendToOthers(Command c) {
    sendToOthers(encoder.encode(c));
  }

  public void sendToAll(String msg) {
    if (isConnected()) {
//      msg = filter.filter(msg, roomMgr.getDefaultRoom().getName(), getRoom().getName());
      sendToOthers(msg);
      showCHAT(((P2PPlayer) me).getInfo(), msg);
    }
  }

  public void sendToOthers(String msg) {
    if (isConnected()) {
      Room myRoom = getRoom();
//      msg = filter.filter(msg, roomMgr.getDefaultRoom().getName(), getRoom().getName());
      Player[] p = (Player[]) myRoom.getPlayerList().toArray();
      for (int i = 0; i < p.length; ++i) {
        if (!p[i].equals(me)) {
          ActivePeer peer = peerMgr.getPeerListenerByID(((P2PPlayer) p[i]).getInfo().getID());
          if (peer != null) {
            peer.sendCHAT(msg);
          }
        }
      }
    }
  }

  public void sendTo(Player recipient, Command c) {
    if (peerMgr != null) {
      peerMgr.getPeerListenerByInfo(((P2PPlayer) recipient).getInfo()).sendCHAT(encoder.encode(c));
    }
  }

  public Room getRoom() {
    return roomMgr.getRoomContaining(me);
  }

  public void setRoom(Room r) {
    if (me instanceof P2PPlayer) {
      ((P2PPlayer) me).setRoom(r.getName());
      if (isConnected()) {
        peerMgr.sendToAllNAME();
        propSupport.firePropertyChange(AVAILABLE_ROOMS, null, roomMgr.update(((P2PPlayer) me).getInfo()));
        Room newRoom = getRoom();
        propSupport.firePropertyChange(ROOM, null, newRoom);
      }
    }
  }

  public Room[] getAvailableRooms() {
    return roomMgr.getRooms();
  }

  public Player getUserInfo() {
    return me;
  }

  public void setUserInfo(Player p) {
    if (me instanceof P2PPlayer) {
      ((P2PPlayer) me).setStats(p);
      if (isConnected()) {
        propSupport.firePropertyChange(AVAILABLE_ROOMS, null, roomMgr.update(((P2PPlayer) me).getInfo()));
        propSupport.firePropertyChange(ROOM, null, getRoom());
        peerMgr.sendToAllNAME();
      }
    }
    else {
      me = p;
    }
    propSupport.firePropertyChange(PLAYER_INFO, null, me);
  }

  public void setConnected(boolean connect) {
    if (connect) {
      try {
        MyInfo info = new MyInfo(null, 5050);
        P2PPlayer p = new P2PPlayer(info);
        p.setName(me.getName());
        p.setRoom(roomMgr.getDefaultRoom().getName());
        p.setId(GameModule.getUserId() + "." + System.currentTimeMillis());
        setUserInfo(p);
        pool.initialize(p, ppm);
        if (peerMgr == null) {
          peerMgr = new ActivePeerManager(info, this, ppm);
        }
        roomMgr.update(((P2PPlayer) me).getInfo());
        propSupport.firePropertyChange(STATUS, null, "Server connection established");
        propSupport.firePropertyChange(CONNECTED, null, Boolean.TRUE);
        propSupport.firePropertyChange(AVAILABLE_ROOMS, null, roomMgr.getRooms());
        propSupport.firePropertyChange(ROOM, null, getRoom());
        welcomeMessageServer.getWelcomeMessage().execute();
        connected = true;
      }
      catch (IOException e) {
        propSupport.firePropertyChange(STATUS, null, "Unable to establish connection: " + e.getMessage());
        propSupport.firePropertyChange(CONNECTED, null, Boolean.FALSE);
        connected = false;
      }
    }
    else if (isConnected()) {
      if (peerMgr != null) {
        peerMgr.clear();
      }
      roomMgr.clear();
      pool.disconnect();
      propSupport.firePropertyChange(CONNECTED, Boolean.TRUE, Boolean.FALSE);
      propSupport.firePropertyChange(AVAILABLE_ROOMS, null, new Room[0]);
      propSupport.firePropertyChange(ROOM, new SimpleRoom(), null);
      connected = false;
    }
  }

  public boolean isConnected() {
    return connected;
  }

  public Message[] getMessages() {
    return msgSvr.getMessages();
  }

  public void postMessage(String msg) {
    msgSvr.postMessage(msg);
  }

  public MessageBoard getMessageServer() {
    return msgSvr;
  }

  public ServerStatus getStatusServer() {
    return svrStatus;
  }

  public Player stringToPlayer(String s) {
    return roomMgr.getPlayerById(s);
  }

  public String playerToString(Player p) {
    return ((P2PPlayer) p).getId();
  }

  public void addPropertyChangeListener(String propertyName, java.beans.PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(propertyName, l);
  }
  
  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  public void setActivePeerManager(ActivePeerManager pActivePeerManager) {
    peerMgr = pActivePeerManager;
  }

  public void setPendingPeerManager(PendingPeerManager pPendingPeerManager) {
    ppm = pPendingPeerManager;
  }

  public synchronized void showUnrecognized(PeerInfo pPeerInfo, String pBadMessage) {
  }

  public synchronized void showStreamsFailed(PeerInfo pPeerInfo) {
    P2PPlayer p = new P2PPlayer(pPeerInfo);
    propSupport.firePropertyChange(STATUS, null, "Lost connection to to " + p.getName());
  }

  public synchronized void showConnectFailed(PeerInfo pPeerInfo) {
    pool.connectFailed(pPeerInfo);
  }

  public synchronized void showConnect(PeerInfo pPeerInfo) {
  }

  public synchronized void showDisconnect(PeerInfo pPeerInfo) {
    propSupport.firePropertyChange(AVAILABLE_ROOMS, null, roomMgr.remove(pPeerInfo));
    propSupport.firePropertyChange(ROOM, null, getRoom());
  }

  public synchronized void showCHAT(PeerInfo pPeerInfo, String msg) {
    propSupport.firePropertyChange(INCOMING_MSG,null,msg);
  }

  public synchronized void showPMSG(PeerInfo pPeerInfo, String msg) {
    showCHAT(pPeerInfo, msg);
  }

  public synchronized void showNAME(PeerInfo pPeerInfo) {
    tracker.init(getRoom());
    propSupport.firePropertyChange(AVAILABLE_ROOMS, null, roomMgr.update(pPeerInfo));
    Room myRoom = getRoom();
    propSupport.firePropertyChange(ROOM, null, myRoom);
    tracker.finalize(myRoom);
  }

  public synchronized void showHELO(PeerInfo pPeerInfo) {
    propSupport.firePropertyChange(AVAILABLE_ROOMS, null, roomMgr.update(pPeerInfo));
    propSupport.firePropertyChange(ROOM, null, getRoom());
  }

/*    final String moduleName = args.length > 0 ? args[0] : "test";
    final String myName = args.length > 1 ? args[1] : "rk";
    final P2PClient client = new P2PClient(new CgiPeerPool(new PeerPoolInfo() {
      public String getModuleName() {
        return moduleName;
      }

      public String getUserName() {
        return myName;
      }
    }, "http://www.vassalengine.org/util/"));
    client.addPropertyChangeListener(ServerConnection.CONNECTED, new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        if (evt.getNewValue() != null
          && evt.getNewValue().getClass().isArray()) {
          Object[] o = (Object[]) evt.getNewValue();
          System.out.println(evt.getPropertyName() + " = ");
          for (int i = 0; i < o.length; ++i) {
            System.out.println("  " + o[i]);
          }
        }
        else {
          System.out.println(evt.getPropertyName() + " = " + evt.getNewValue());
        }
      }
    });
    ChatServerControls w = new ChatServerControls();
    w.setClient(client);
    final javax.swing.JTextField tf = new javax.swing.JTextField();
    tf.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        client.sendToOthers(evt.getActionCommand());
        tf.setText("");
      }
    });
    JFrame f = new JFrame();
    f.getContentPane().setLayout(new BorderLayout());
    f.getContentPane().add(w.getControls());
    f.getContentPane().add(tf,BorderLayout.SOUTH);
    f.pack();
    f.setVisible(true);
  }
*/

}
