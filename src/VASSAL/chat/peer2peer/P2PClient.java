/*
 * $Id:
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
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
package VASSAL.chat.peer2peer;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.util.Properties;

import org.litesoft.p2pchat.ActivePeer;
import org.litesoft.p2pchat.ActivePeerManager;
import org.litesoft.p2pchat.MyInfo;
import org.litesoft.p2pchat.PeerInfo;
import org.litesoft.p2pchat.PendingPeerManager;
import org.litesoft.p2pchat.UserDialog;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.PlayerEncoder;
import VASSAL.chat.Room;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.SimpleStatus;
import VASSAL.chat.SoundEncoder;
import VASSAL.chat.SynchEncoder;
import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.messageboard.Message;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.ui.ChatControlsInitializer;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.chat.ui.RoomInteractionControlsInitializer;
import VASSAL.chat.ui.ShowProfileAction;
import VASSAL.chat.ui.SimpleStatusControlsInitializer;
import VASSAL.chat.ui.SynchAction;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.i18n.Resources;
import VASSAL.tools.PropertiesEncoder;

public class P2PClient implements ChatServerConnection, ChatControlsInitializer, UserDialog, PlayerEncoder {
  private SimplePlayer me;
  private PendingPeerManager ppm;
  protected ActivePeerManager peerMgr;
  private PeerPool pool;
  private MessageBoard msgSvr;
  private WelcomeMessageServer welcomeMessageServer;
  private RoomManager roomMgr;
  private RoomTracker tracker;
  private PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  private CommandEncoder encoder;
  private boolean connected = false;
  private ServerStatus svrStatus;
  private RoomInteractionControlsInitializer roomControls;
  private SimpleStatusControlsInitializer playerStatusControls;
  private SoundEncoder soundEncoder;
  private SynchEncoder synchEncoder;
  private PropertyChangeListener nameChangeListener;
  private Properties params;

  public P2PClient(CommandEncoder encoder, MessageBoard msgSvr, WelcomeMessageServer welcomeMessageServer, PeerPool pool) {
    this(encoder, msgSvr, welcomeMessageServer, pool, new Properties());
  }

  public P2PClient(CommandEncoder encoder, MessageBoard msgSvr, WelcomeMessageServer welcomeMessageServer, PeerPool pool, Properties param) {
    this.encoder = encoder;
    this.msgSvr = msgSvr;
    this.welcomeMessageServer = welcomeMessageServer;
    this.pool = pool;
    this.params = param;
    ppm = new PendingPeerManager(this);
    ppm.setName("Pending Peer Manager"); //$NON-NLS-1$
    roomMgr = new RoomManager();
    tracker = new RoomTracker();
    me = new SimplePlayer("???"); //$NON-NLS-1$
    me.updateStatus();
    playerStatusControls = new SimpleStatusControlsInitializer(this, false);
    roomControls = new RoomInteractionControlsInitializer(this);
    roomControls.addPlayerActionFactory(ShowProfileAction.factory());
    roomControls.addPlayerActionFactory(SynchAction.factory(this));
    synchEncoder = new SynchEncoder(this, this);
    soundEncoder = new SoundEncoder(this);
    nameChangeListener = new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        SimplePlayer p = (SimplePlayer) getUserInfo();
        p.setName((String) evt.getNewValue());
        setUserInfo(p);
      }
    };
  }

  public RoomManager getRoomMgr() {
    return roomMgr;
  }

  public void sendToOthers(Command c) {
    sendToOthers(encoder.encode(c));
  }

  public void sendToAll(String msg) {
    if (isConnected()) {
      sendToOthers(msg);
      showCHAT(((P2PPlayer) me).getInfo(), msg);
    }
  }

  public void sendToOthers(String msg) {
    if (isConnected()) {
      final Room myRoom = getRoom();
      final Player[] pl = myRoom.getPlayerList().toArray(
        new Player[myRoom.getPlayerList().size()]);
      for (Player p : pl) {
        if (!p.equals(me)) {
          final ActivePeer peer =
            peerMgr.getPeerListenerByID(((P2PPlayer) p).getInfo().getID());
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
      me = (SimplePlayer) p;
      me.updateStatus();
    }
    propSupport.firePropertyChange(PLAYER_INFO, null, me);
  }

  public void setConnected(boolean connect) {
    if (connect) {
      try {
        Integer port;
        try {
          port = Integer.valueOf(params.getProperty(P2PClientFactory.P2P_LISTEN_PORT));
        }
        catch (NumberFormatException ex) {
          port = Integer.valueOf(5050);
        }
        MyInfo info = new MyInfo(null, port);
        info.setNetworkPw(params.getProperty(P2PClientFactory.P2P_SERVER_PW));
        P2PPlayer p = new P2PPlayer(info);
        p.updateStatus();
        p.setName(me.getName());
        p.setRoom(roomMgr.getDefaultRoom().getName());
        p.setId(GameModule.getUserId() + "." + System.currentTimeMillis()); //$NON-NLS-1$
        setUserInfo(p);
        pool.initialize(p, ppm);
        if (peerMgr == null) {
          peerMgr = new ActivePeerManager(info, this, ppm);
        }
        roomMgr.update(((P2PPlayer) me).getInfo());
        fireStatus(Resources.getString("Peer2Peer.server_connection_established", params.getProperty(P2PClientFactory.P2P_LISTEN_PORT))); //$NON-NLS-1$
        propSupport.firePropertyChange(AVAILABLE_ROOMS, null, roomMgr.getRooms());
        propSupport.firePropertyChange(ROOM, null, getRoom());
        welcomeMessageServer.getWelcomeMessage().execute();
        connected = true;
        propSupport.firePropertyChange(CONNECTED, null, Boolean.TRUE);
      }
      // FIXME: review error message
      catch (IOException e) {
        fireStatus(Resources.getString("Peer2Peer.connection_error", e.getMessage())); //$NON-NLS-1$
        fireStatus(Resources.getString("Peer2Peer.disconnected")); //$NON-NLS-1$ //$NON-NLS-2$
        connected = false;
        propSupport.firePropertyChange(CONNECTED, null, Boolean.FALSE);
      }
    }
    else if (isConnected()) {
      if (peerMgr != null) {
        peerMgr.clear();
      }
      roomMgr.clear();
      pool.disconnect();
      propSupport.firePropertyChange(AVAILABLE_ROOMS, null, new Room[0]);
      propSupport.firePropertyChange(ROOM, new SimpleRoom(), null);
      connected = false;
      propSupport.firePropertyChange(CONNECTED, Boolean.TRUE, Boolean.FALSE);
      fireStatus(Resources.getString("Peer2Peer.disconnected")); //$NON-NLS-1$ //$NON-NLS-2$
      ppm.finish();
    }
  }

  protected void fireStatus(String msg) {
    propSupport.firePropertyChange(STATUS, null, msg);
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
    return p.getId();
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
    propSupport.firePropertyChange(STATUS, null, Resources.getString("Peer2Peer.connection_lost", p.getName())); //$NON-NLS-1$
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
    // We have received a connection request
    final Chatter chatter = GameModule.getGameModule().getChatter();
    final ActivePeer peer = peerMgr.getPeerListenerByInfo(pPeerInfo);
    Properties props;
    String name, ip, details;
    try {
      props = new PropertiesEncoder(pPeerInfo.getChatName()).getProperties();
      name = props.getProperty(SimpleStatus.NAME);
      ip = props.getProperty(SimpleStatus.IP);
      details = name+" ("+ip+":"+pPeerInfo.getPort()+")";
    }
    catch (IOException ex) {
      details = "";
    }

    // Does the password of the new Peer match ours?
    if (! pPeerInfo.getNetworkPw().equals(params.getProperty(P2PClientFactory.P2P_SERVER_PW))) {
      new Chatter.DisplayText(chatter, Resources.getString("Peer2Peer.bad_password", details)).execute();
      peer.finish();
      return;
    }

    fireStatus(Resources.getString("Peer2Peer.connected", details));

    propSupport.firePropertyChange(AVAILABLE_ROOMS, null, roomMgr.update(pPeerInfo));
    propSupport.firePropertyChange(ROOM, null, getRoom());
  }

  public void initializeControls(ChatServerControls controls) {
    playerStatusControls.initializeControls(controls);
    roomControls.initializeControls(controls);
    controls.setRoomControlsVisible(false);
    final GameModule g = GameModule.getGameModule();
    me.setName((String) g.getPrefs().getValue(GameModule.REAL_NAME));
    g.getPrefs().getOption(GameModule.REAL_NAME).addPropertyChangeListener(nameChangeListener);
    g.addCommandEncoder(synchEncoder);
    g.addCommandEncoder(soundEncoder);
    if (pool instanceof ChatControlsInitializer) {
      ((ChatControlsInitializer)pool).initializeControls(controls);
    }
  }

  public void uninitializeControls(ChatServerControls controls) {
    playerStatusControls.uninitializeControls(controls);
    roomControls.uninitializeControls(controls);
    final GameModule g = GameModule.getGameModule();
    g.getPrefs().getOption(GameModule.REAL_NAME).removePropertyChangeListener(nameChangeListener);
    g.removeCommandEncoder(synchEncoder);
    g.removeCommandEncoder(soundEncoder);
    if (pool instanceof ChatControlsInitializer) {
      ((ChatControlsInitializer)pool).uninitializeControls(controls);
    }
  }

}
