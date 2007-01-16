/*
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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
package VASSAL.chat.node;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.util.Properties;
import VASSAL.build.GameModule;
import VASSAL.chat.Base64;
import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Compressor;
import VASSAL.chat.MainRoomChecker;
import VASSAL.chat.Player;
import VASSAL.chat.PlayerEncoder;
import VASSAL.chat.PrivateChatEncoder;
import VASSAL.chat.PrivateChatManager;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.SimpleStatus;
import VASSAL.chat.SoundEncoder;
import VASSAL.chat.SynchEncoder;
import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.messageboard.Message;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.ui.BasicChatControlsInitializer;
import VASSAL.chat.ui.ChatControlsInitializer;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.chat.ui.LockableRoomControls;
import VASSAL.chat.ui.LockableRoomTreeRenderer;
import VASSAL.chat.ui.MessageBoardControlsInitializer;
import VASSAL.chat.ui.PrivateMessageAction;
import VASSAL.chat.ui.RoomInteractionControlsInitializer;
import VASSAL.chat.ui.SendSoundAction;
import VASSAL.chat.ui.ServerStatusControlsInitializer;
import VASSAL.chat.ui.ShowProfileAction;
import VASSAL.chat.ui.SimpleStatusControlsInitializer;
import VASSAL.chat.ui.SynchAction;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.tools.PropertiesEncoder;
import VASSAL.tools.SequenceEncoder;

/**
 * @author rkinney
 */
public abstract class NodeClient implements ChatServerConnection, PlayerEncoder, ChatControlsInitializer {
  public static final String ZIP_HEADER = "!ZIP!";
  protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  protected NodePlayer me;
  protected SimpleRoom currentRoom;
  protected String defaultRoomName = "Main Room";
  protected NodeRoom[] allRooms = new NodeRoom[0];
  protected MessageBoard msgSvr;
  protected WelcomeMessageServer welcomer;
  protected ServerStatus serverStatus;
  protected String moduleName;
  protected String playerId;
  protected MainRoomChecker checker = new MainRoomChecker();
  protected int compressionLimit = 1000;
  protected CommandEncoder encoder;
  protected MessageBoardControlsInitializer messageBoardControls;
  protected BasicChatControlsInitializer basicControls;
  protected RoomInteractionControlsInitializer roomControls;
  protected ServerStatusControlsInitializer serverStatusControls;
  protected SimpleStatusControlsInitializer playerStatusControls;
  protected SoundEncoder soundEncoder;
  protected PrivateChatEncoder privateChatEncoder;
  protected SynchEncoder synchEncoder;
  protected PropertyChangeListener nameChangeListener;
  protected PropertyChangeListener profileChangeListener;

  public NodeClient(String moduleName, String playerId, CommandEncoder encoder, MessageBoard msgSvr, WelcomeMessageServer welcomer) {
    this.encoder = encoder;
    this.msgSvr = msgSvr;
    this.welcomer = welcomer;
    this.playerId = playerId;
    this.moduleName = moduleName;
    serverStatus = new CgiServerStatus();
    me = new NodePlayer(playerId);
    messageBoardControls = new MessageBoardControlsInitializer("Messages", msgSvr);
    basicControls = new BasicChatControlsInitializer(this);
    roomControls = new LockableRoomControls(this);
    roomControls.addPlayerActionFactory(ShowProfileAction.factory());
    roomControls.addPlayerActionFactory(SynchAction.factory(this));
    roomControls.addPlayerActionFactory(PrivateMessageAction.factory(this, new PrivateChatManager(this)));
    roomControls.addPlayerActionFactory(SendSoundAction.factory(this, "Send Wake-up", "wakeUpSound", "phone1.wav"));
    serverStatusControls = new ServerStatusControlsInitializer(serverStatus);
    playerStatusControls = new SimpleStatusControlsInitializer(this);
    synchEncoder = new SynchEncoder(this, this);
    privateChatEncoder = new PrivateChatEncoder(this, new PrivateChatManager(this));
    soundEncoder = new SoundEncoder();
    nameChangeListener = new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        SimplePlayer p = (SimplePlayer) getUserInfo();
        p.setName((String) evt.getNewValue());
        setUserInfo(p);
      }
    };
    profileChangeListener = new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        SimplePlayer p = (SimplePlayer) getUserInfo();
        SimpleStatus s = (SimpleStatus) p.getStatus();
        s = new SimpleStatus(s.isLooking(), s.isAway(), (String) evt.getNewValue());
        p.setStatus(s);
        setUserInfo(p);
      }
    };
  }

  public void setConnected(boolean connect) {
    if (connect) {
      if (!isConnected()) {
        try {
          NodePlayer oldPlayer = me;
          me = new NodePlayer(playerId);
          setUserInfo(oldPlayer);
          initializeConnection();
          Command welcomeMessage = welcomer.getWelcomeMessage();
          if (welcomeMessage != null) {
            welcomeMessage.execute();
          }
          registerNewConnection();
        }
        catch (IOException e) {
          propSupport.firePropertyChange(STATUS, null, "Unable to establish connection to server:  " + e.getMessage());
        }
      }
    }
    else {
      if (isConnected()) {
        closeConnection();
      }
      currentRoom = null;
      allRooms = new NodeRoom[0];
    }
    propSupport.firePropertyChange(CONNECTED, null, isConnected() ? Boolean.TRUE : Boolean.FALSE);
  }

  protected void registerNewConnection() {
    String path = new SequenceEncoder(moduleName, '/').append(defaultRoomName).getValue();
    send(Protocol.encodeRegisterCommand(me.getId(), path, new PropertiesEncoder(me.toProperties()).getStringValue()));
    if (GameModule.getGameModule() != null) {
      String username = (String) GameModule.getGameModule().getPrefs().getValue("Login");
      if (username != null) {
        send(Protocol.encodeLoginCommand(username));
      }
    }
  }

  protected abstract void closeConnection();

  protected abstract void initializeConnection() throws IOException;

  public abstract void send(String command);

  public void setDefaultRoomName(String defaultRoomName) {
    this.defaultRoomName = defaultRoomName;
  }

  protected void sendStats() {
    if (isConnected()) {
      send(Protocol.encodeStatsCommand(new PropertiesEncoder(me.toProperties()).getStringValue()));
    }
  }

  public void sendToOthers(Command c) {
    sendToOthers(encoder.encode(c));
  }

  public void sendToAll(String msg) {
    if (currentRoom != null) {
      String path = new SequenceEncoder(moduleName, '/').append(currentRoom.getName()).getValue();
      forward(path, msg);
    }
  }

  public void forward(String receipientPath, String msg) {
    if (isConnected() && currentRoom != null && msg != null) {
      msg = checker.filter(msg, defaultRoomName, currentRoom.getName());
      if (msg.length() > compressionLimit) {
        try {
          msg = ZIP_HEADER + Base64.encodeBytes(Compressor.compress(msg.getBytes("UTF-8")), false);
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
      send(Protocol.encodeForwardCommand(receipientPath, msg));
    }
  }

  public void sendToOthers(String msg) {
    if (currentRoom != null) {
      String path = new SequenceEncoder(moduleName, '/').append(currentRoom.getName()).append("~" + me.getId()).getValue();
      forward(path, msg);
    }
  }

  public void sendTo(Player recipient, Command c) {
    String path = new SequenceEncoder(moduleName, '/').append("*").append(((NodePlayer) recipient).getId()).getValue();
    forward(path, encoder.encode(c));
  }

  public VASSAL.chat.Room getRoom() {
    return currentRoom;
  }

  public VASSAL.chat.Room[] getAvailableRooms() {
    return allRooms;
  }

  public void addPropertyChangeListener(String propertyName, PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(propertyName, l);
  }

  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  public Player getUserInfo() {
    return me;
  }

  public NodePlayer getMyInfo() {
    return me;
  }

  public void setUserInfo(Player p) {
    me.setName(p.getName());
    me.setStatus(p.getStatus());
    sendStats();
    propSupport.firePropertyChange(PLAYER_INFO, null, me);
  }

  public void lockRoom(NodeRoom r) {
    if (r.getOwner().equals(me.getId())) {
      r.lock();
      sendRoomInfo(r);
    }
  }

  public void sendRoomInfo(NodeRoom r) {
    Node dummy = new Node(null, r.getName(), new PropertiesEncoder(r.getInfo()).getStringValue());
    if (isConnected()) {
      String msg = Protocol.encodeRoomsInfo(new Node[]{dummy});
      send(msg);
    }
  }

  public void setRoom(VASSAL.chat.Room r) {
    if (isConnected()) {
      String newRoom = r.getName();
      String newPath = new SequenceEncoder(moduleName, '/').append(newRoom).getValue();
      String msg = Protocol.encodeJoinCommand(newPath);
      send(msg);
    }
  }

  public void handleMessageFromServer(String msg) {
    Node n;
    Properties p;
    if ((n = Protocol.decodeListCommand(msg)) != null) {
      Node mod = n.getChild(moduleName);
      if (mod != null) {
        updateRooms(mod);
      }
    }
    else if ((p = Protocol.decodeRoomsInfo(msg)) != null) {
      for (int i = 0; i < allRooms.length; ++i) {
        String infoString = p.getProperty(allRooms[i].getName());
        if (infoString != null && infoString.length() > 0) {
          try {
            Properties info = new PropertiesEncoder(infoString).getProperties();
            allRooms[i].setInfo(info);
          }
          catch (IOException e) {
            e.printStackTrace();
          }
        }
      }
      propSupport.firePropertyChange(ROOM, null, currentRoom);
      propSupport.firePropertyChange(AVAILABLE_ROOMS, null, allRooms);
    }
    else if (Protocol.decodeRegisterRequest(msg)) {
      registerNewConnection();
    }
    else {
      if (msg.startsWith(ZIP_HEADER)) {
        try {
          msg = new String(Compressor.decompress(Base64.decode(msg.substring(ZIP_HEADER.length()))), "UTF-8");
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
      propSupport.firePropertyChange(INCOMING_MSG, null, msg);
    }
  }

  protected void updateRooms(Node module) {
    Node[] roomNodes = module.getChildren();
    NodeRoom[] rooms = new NodeRoom[roomNodes.length];
    int defaultRoomIndex = -1;
    for (int i = 0; i < roomNodes.length; ++i) {
      Node[] playerNodes = roomNodes[i].getChildren();
      NodePlayer[] players = new NodePlayer[playerNodes.length];
      boolean containsMe = false;
      for (int j = 0; j < playerNodes.length; ++j) {
        players[j] = new NodePlayer(playerNodes[j].getId());
        if (players[j].equals(me)) {
          containsMe = true;
        }
        try {
          Properties p = new PropertiesEncoder(playerNodes[j].getInfo()).getProperties();
          players[j].setInfo(p);
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
      rooms[i] = new NodeRoom(roomNodes[i].getId(), players);
      try {
        if (roomNodes[i].getInfo() != null) {
          rooms[i].setInfo(new PropertiesEncoder(roomNodes[i].getInfo()).getProperties());
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      if (containsMe) {
        currentRoom = rooms[i];
      }
      if (defaultRoomName.equals(rooms[i].getName())) {
        defaultRoomIndex = i;
      }
    }
    if (defaultRoomIndex < 0) {
      allRooms = new NodeRoom[rooms.length + 1];
      System.arraycopy(rooms, 0, allRooms, 1, rooms.length);
      allRooms[0] = new NodeRoom(defaultRoomName);
    }
    else {
      allRooms = rooms;
      NodeRoom swap = allRooms[0];
      allRooms[0] = allRooms[defaultRoomIndex];
      allRooms[defaultRoomIndex] = swap;
    }
    propSupport.firePropertyChange(ROOM, null, currentRoom);
    propSupport.firePropertyChange(AVAILABLE_ROOMS, null, allRooms);
  }

  public MessageBoard getMessageServer() {
    return msgSvr;
  }

  public Message[] getMessages() {
    return msgSvr.getMessages();
  }

  public void postMessage(String msg) {
    msgSvr.postMessage(msg);
  }

  public Player stringToPlayer(String s) {
    NodePlayer p = null;
    try {
      PropertiesEncoder propEncoder = new PropertiesEncoder(s);
      p = new NodePlayer(null);
      p.setInfo(propEncoder.getProperties());
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    return p;
  }

  public String playerToString(Player p) {
    Properties props = ((NodePlayer) p).toProperties();
    return new PropertiesEncoder(props).getStringValue();
  }

  public void initializeControls(ChatServerControls controls) {
    basicControls.initializeControls(controls);
    playerStatusControls.initializeControls(controls);
    messageBoardControls.initializeControls(controls);
    roomControls.initializeControls(controls);
    serverStatusControls.initializeControls(controls);
    GameModule.getGameModule().addCommandEncoder(synchEncoder);
    GameModule.getGameModule().addCommandEncoder(privateChatEncoder);
    GameModule.getGameModule().addCommandEncoder(soundEncoder);
    me.setName((String) GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME));
    GameModule.getGameModule().getPrefs().getOption(GameModule.REAL_NAME).addPropertyChangeListener(nameChangeListener);
    SimpleStatus s = (SimpleStatus) me.getStatus();
    s = new SimpleStatus(s.isLooking(), s.isAway(), (String) GameModule.getGameModule().getPrefs().getValue(GameModule.PERSONAL_INFO));
    me.setStatus(s);
    GameModule.getGameModule().getPrefs().getOption(GameModule.PERSONAL_INFO).addPropertyChangeListener(profileChangeListener);
    controls.getRoomTree().setCellRenderer(new LockableRoomTreeRenderer());
  }

  public void uninitializeControls(ChatServerControls controls) {
    messageBoardControls.uninitializeControls(controls);
    basicControls.uninitializeControls(controls);
    roomControls.uninitializeControls(controls);
    serverStatusControls.uninitializeControls(controls);
    playerStatusControls.uninitializeControls(controls);
    GameModule.getGameModule().removeCommandEncoder(synchEncoder);
    GameModule.getGameModule().removeCommandEncoder(privateChatEncoder);
    GameModule.getGameModule().removeCommandEncoder(soundEncoder);
    GameModule.getGameModule().getPrefs().getOption(GameModule.REAL_NAME).removePropertyChangeListener(nameChangeListener);
    GameModule.getGameModule().getPrefs().getOption(GameModule.PERSONAL_INFO).removePropertyChangeListener(profileChangeListener);
  }
}