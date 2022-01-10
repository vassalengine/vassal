/*
 * Copyright (c) 2000-2020 by Rodney Kinney, Brent Easton, Joel Uckelman
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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.util.Properties;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.ArrayUtils;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.chat.Compressor;
import VASSAL.chat.InviteCommand;
import VASSAL.chat.InviteEncoder;
import VASSAL.chat.LockableChatServerConnection;
import VASSAL.chat.LockableRoom;
import VASSAL.chat.MainRoomChecker;
import VASSAL.chat.Player;
import VASSAL.chat.PlayerEncoder;
import VASSAL.chat.PrivateChatEncoder;
import VASSAL.chat.PrivateChatManager;
import VASSAL.chat.Room;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleRoom;
import VASSAL.chat.SimpleStatus;
import VASSAL.chat.SoundEncoder;
import VASSAL.chat.SynchEncoder;
import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.ui.ChatControlsInitializer;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.chat.ui.InviteAction;
import VASSAL.chat.ui.KickAction;
import VASSAL.chat.ui.LockableRoomTreeRenderer;
import VASSAL.chat.ui.PrivateMessageAction;
import VASSAL.chat.ui.RoomInteractionControlsInitializer;
import VASSAL.chat.ui.SendSoundAction;
import VASSAL.chat.ui.ShowProfileAction;
import VASSAL.chat.ui.SimpleStatusControlsInitializer;
import VASSAL.chat.ui.SynchAction;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.i18n.Resources;
import VASSAL.tools.PropertiesEncoder;
import VASSAL.tools.SequenceEncoder;

/**
 * @author rkinney
 */
public class NodeClient implements LockableChatServerConnection,
    PlayerEncoder, ChatControlsInitializer, SocketWatcher {
  public static final String ZIP_HEADER = "!ZIP!"; //$NON-NLS-1$
  protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  protected NodePlayer me;
  protected SimpleRoom currentRoom;
  protected String defaultRoomName = DEFAULT_ROOM_NAME; //$NON-NLS-1$
  protected NodeRoom[] allRooms = new NodeRoom[0];
  protected String moduleName;
  protected String playerId;
  protected MainRoomChecker checker = new MainRoomChecker();
  protected int compressionLimit = 1000;
  protected CommandEncoder encoder;
  protected RoomInteractionControlsInitializer roomControls;
  protected SimpleStatusControlsInitializer playerStatusControls;
  protected SoundEncoder soundEncoder;
  protected PrivateChatEncoder privateChatEncoder;
  protected SynchEncoder synchEncoder;
  protected InviteEncoder inviteEncoder;
  protected PropertyChangeListener nameChangeListener;
  protected PropertyChangeListener profileChangeListener;
  protected NodeRoom pendingSynchToRoom;

  private SocketHandler sender;

  protected final String host;
  protected final int port;

  protected final WelcomeMessageServer welcomer;

  public NodeClient(String moduleName, String playerId, CommandEncoder encoder,  String host, int port, WelcomeMessageServer welcomer) {

    this.host = host;
    this.port = port;

    this.encoder = encoder;
    this.welcomer = welcomer;
    this.playerId = playerId;
    this.moduleName = moduleName;

    me = new NodePlayer(playerId);
    roomControls = new LockableNodeRoomControls(this);
    roomControls.addPlayerActionFactory(ShowProfileAction.factory());
    roomControls.addPlayerActionFactory(SynchAction.factory(this));
    final PrivateChatManager privateChatManager = new PrivateChatManager(this);
    roomControls.addPlayerActionFactory(PrivateMessageAction.factory(this,
        privateChatManager));
    roomControls.addPlayerActionFactory(SendSoundAction.factory(this, Resources
        .getString("Chat.send_wakeup"), "wakeUpSound", "phone1.wav")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    roomControls.addPlayerActionFactory(InviteAction.factory(this));
    roomControls.addPlayerActionFactory(KickAction.factory(this));
    playerStatusControls = new SimpleStatusControlsInitializer(this);
    synchEncoder = new SynchEncoder(this, this);
    privateChatEncoder = new PrivateChatEncoder(this, privateChatManager);
    soundEncoder = new SoundEncoder(this);
    inviteEncoder = new InviteEncoder(this);

    nameChangeListener = e -> {
      final SimplePlayer p = (SimplePlayer) getUserInfo();
      p.setName((String) e.getNewValue());
      setUserInfo(p);
    };

    profileChangeListener = e -> {
      final SimplePlayer p = (SimplePlayer) getUserInfo();
      SimpleStatus s = (SimpleStatus) p.getStatus();
      s = new SimpleStatus(
        s.isLooking(),
        s.isAway(),
        (String) e.getNewValue(),
        s.getClient(),
        s.getIp(),
        s.getModuleVersion(),
        s.getCrc()
      );
      p.setStatus(s);
      setUserInfo(p);
    };
  }

  @Override
  public void setConnected(boolean connect) {
    if (connect) {
      if (!isConnected()) {
        try {
          final NodePlayer oldPlayer = me;
          me = new NodePlayer(playerId);
          setUserInfo(oldPlayer);
          initializeConnection();
          final Command welcomeMessage = welcomer.getWelcomeMessage();
          if (welcomeMessage != null) {
            welcomeMessage.execute();
          }
          registerNewConnection();
        }
        // FIXME: review error message
        catch (final IOException e) {
          propSupport.firePropertyChange(STATUS, null, Resources.getString(
              "Chat.unable_to_establish", e.getMessage())); //$NON-NLS-1$
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
    propSupport.firePropertyChange(CONNECTED, null,
        isConnected() ? Boolean.TRUE : Boolean.FALSE);
  }

  protected void registerNewConnection() {
    synchronized (sender) {
      if (sender == null) { // If already hung up
        return;
      }
      final String path = new SequenceEncoder(moduleName, '/').append(defaultRoomName)
        .getValue();
      send(Protocol.encodeRegisterCommand(me.getId(), path,
        new PropertiesEncoder(me.toProperties()).getStringValue()));
      if (GameModule.getGameModule() != null) {
        final String username = (String) GameModule.getGameModule().getPrefs()
          .getValue("Login"); //$NON-NLS-1$
        if (username != null) {
          send(Protocol.encodeLoginCommand(username));
        }
      }
    }
  }

  protected void initializeConnection() throws UnknownHostException, IOException {
    final Socket s = new Socket(host, port);
    synchronized (sender) {
      sender = new SocketHandler(s, this);
      sender.start();
    }
  }

  protected void closeConnection() {
    final SocketHandler s;
    synchronized (sender) {
      s = sender;
      sender = null;
    }
    s.close();
  }

  @Override
  public boolean isConnected() {
    synchronized (sender) {
      return sender != null;
    }
  }

  @Override
  public void socketClosed(SocketHandler handler) {
    synchronized (sender) {
      if (sender != null) {
        propSupport.firePropertyChange(STATUS, null, Resources.getString("Server.lost_connection")); //$NON-NLS-1$
        propSupport.firePropertyChange(CONNECTED, null, Boolean.FALSE);
        sender = null;
      }
    }
  }

  public void send(String command) {
    synchronized (sender) {
      sender.writeLine(command);
    }
  }

  public void setDefaultRoomName(String defaultRoomName) {
    this.defaultRoomName = defaultRoomName;
  }

  @Override
  public String getDefaultRoomName() {
    return defaultRoomName;
  }

  @Override
  public boolean isDefaultRoom(Room r) {
    return r != null && r.getName().equals(getDefaultRoomName());
  }

  protected void sendStats() {
    if (isConnected()) {
      send(Protocol.encodeStatsCommand(new PropertiesEncoder(me.toProperties())
          .getStringValue()));
    }
  }

  @Override
  public void sendToOthers(Command c) {
    sendToOthers(encoder.encode(c));
  }

  public void sendToAll(String msg) {
    if (currentRoom != null) {
      final String path = new SequenceEncoder(moduleName, '/').append(
          currentRoom.getName()).getValue();
      forward(path, msg);
    }
  }

  public void forward(String receipientPath, String msg) {
    if (isConnected() && currentRoom != null && msg != null) {
      msg = checker.filter(msg, defaultRoomName, currentRoom.getName());
      if (msg.length() > compressionLimit) {
        try {
          msg = ZIP_HEADER + Base64.encodeBase64String(
            Compressor.compress(msg.getBytes(StandardCharsets.UTF_8))
          );
        }
        // FIXME: review error message
        catch (final IOException e) {
          e.printStackTrace();
        }
      }
      send(Protocol.encodeForwardCommand(receipientPath, msg));
    }
  }

  public void sendToOthers(String msg) {
    if (currentRoom != null) {
      final String path = new SequenceEncoder(moduleName, '/').append(
          currentRoom.getName()).append("~" + me.getId()).getValue(); //$NON-NLS-1$
      forward(path, msg);
    }
  }

  @Override
  public void sendTo(Player recipient, Command c) {
    final String path = new SequenceEncoder(moduleName, '/')
      .append("*").append(recipient.getId()).getValue(); //$NON-NLS-1$
    forward(path, encoder.encode(c));
  }

  @Override
  public void doKick(Player kickee) {
    send(Protocol.encodeKickCommand(kickee.getId()));
  }

  @Override
  public boolean isKickable(Player kickee) {
    if (kickee != null) {
      final Room room = getRoom();
      // Is this a locked room?
      if (room instanceof LockableRoom && ((LockableRoom) room).isLocked()) {
        if (room instanceof NodeRoom) {
          // Is the target player in the same room?
          if (((NodeRoom) room).contains(kickee)) {
            final String owner = ((NodeRoom) room).getOwner();
            // Do I own this room and the target is not me?
            if (owner != null && owner.equals(getUserInfo().getId())
                && !owner.equals(kickee.getId())) {
              return true;
            }
          }
        }
      }
    }
    return false;
  }

  @Override
  public boolean isInvitable(Player invitee) {
    if (invitee != null) {
      final Room room = getRoom();
      if (room instanceof NodeRoom) {
        // Is the target player in a different room?
        if (!((NodeRoom) room).contains(invitee)) {
          final String owner = ((NodeRoom) room).getOwner();
          // Do I own this room and the target is not me?
          if (owner != null && owner.equals(getUserInfo().getId())
              && !owner.equals(invitee.getId())) {
            return true;
          }
        }
      }
    }
    return false;
  }

  /**
   * Send Invitation to another player to join the current room
   *
   * @param invitee
   *          Player to invite
   */
  @Override
  public void sendInvite(Player invitee) {
    sendTo(invitee, new InviteCommand(me.getName(), me.getId(), getRoom()
        .getName()));
  }

  /**
   * Process an invitation request from a player to join a room
   *
   * @param playerId Inviting player name
   * @param roomName Inviting room
   */
  @Override
  public void doInvite(String playerId, String roomName) {
    for (final Room room : getAvailableRooms()) {
      if (room.getName().equals(roomName)) {
        if (room instanceof NodeRoom) {
          final String owner = ((NodeRoom) room).getOwner();
          if (owner != null && owner.equals(playerId)) {
            setRoom(room, playerId);
            return;
          }
        }
      }
    }
  }

  @Override
  public Room getRoom() {
    return currentRoom;
  }

  @Override
  public Room[] getAvailableRooms() {
    return allRooms;
  }

  @Override
  public void addPropertyChangeListener(String propertyName,
                                        PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(propertyName, l);
  }

  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  @Override
  public Player getUserInfo() {
    return me;
  }

  public NodePlayer getMyInfo() {
    return me;
  }

  @Override
  public void setUserInfo(Player p) {
    me.setName(p.getName());
    me.setStatus(p.getStatus());
    sendStats();
    propSupport.firePropertyChange(PLAYER_INFO, null, me);
  }

  @Override
  public void lockRoom(LockableRoom r) {
    if (r instanceof NodeRoom) {
      final NodeRoom n = (NodeRoom) r;
      if (n.getOwner().equals(me.getId())) {
        n.toggleLock();
        sendRoomInfo(n);
        propSupport.firePropertyChange(AVAILABLE_ROOMS, null, allRooms);
      }
    }
  }

  public void sendRoomInfo(NodeRoom r) {
    final Node dummy = new Node(null, r.getName(), new PropertiesEncoder(r.getInfo())
        .getStringValue());
    if (isConnected()) {
      final String msg = Protocol.encodeRoomsInfo(new Node[] { dummy });
      send(msg);
    }
  }

  @Override
  public void setRoom(Room r) {
    setRoom(r, null);
  }

  public void setRoom(Room r, String password) {
    if (isConnected()) {
      final String newRoom = r.getName();
      final String newPath = new SequenceEncoder(moduleName, '/').append(
          newRoom).getValue();
      final String msg = Protocol.encodeJoinCommand(newPath, password);
      send(msg);
      // Request a synch if we are not the owner
      if (r instanceof NodeRoom) {
        final NodeRoom room = (NodeRoom) r;
        if (newRoom.equals(defaultRoomName)) {
          GameModule.getGameModule().setGameFileMode(GameModule.GameFileMode.NEW_GAME);
          GameModule.getGameModule().getGameState().setup(false);
        }
        else if (!room.isOwner(me)) {
          // We are not actually recorded as being in the new room until we get
          // an update back from
          // the server. Record a Synch required to the new room.
          pendingSynchToRoom = room;
          GameModule.getGameModule().warn(
              Resources.getString("Chat.synchronize_pending"));
        }
      }
    }
  }

  /**
   * Process a message received from the server
   *
   * @param msg
   *          Encoded message
   */
  public void handleMessageFromServer(String msg) {
    final Node n;
    final Properties p;
    if ((n = Protocol.decodeListCommand(msg)) != null) {
      final Node mod = n.getChild(moduleName);
      if (mod != null) {
        updateRooms(mod);
      }
      // Rooms have been updated with any new players (including us), so perform
      // a Synchronize
      // for a move to a new room if needed.
      if (pendingSynchToRoom != null) {
        new SynchAction(pendingSynchToRoom.getOwningPlayer(), this)
            .actionPerformed(null);
        pendingSynchToRoom = null;
        GameModule.getGameModule().warn(
            Resources.getString("Chat.synchronize_complete"));
      }
    }
    else if ((p = Protocol.decodeRoomsInfo(msg)) != null) {
      for (final NodeRoom aRoom : allRooms) {
        final String infoString = p.getProperty(aRoom.getName());
        if (infoString != null && infoString.length() > 0) {
          try {
            final Properties info = new PropertiesEncoder(infoString).getProperties();
            aRoom.setInfo(info);
          }
          // FIXME: review error message
          catch (final IOException e) {
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
          msg = new String(
            Compressor.decompress(
              Base64.decodeBase64(
                msg.substring(ZIP_HEADER.length())
              )
            ),
            StandardCharsets.UTF_8
          );
        }
        // FIXME: review error message
        catch (final IOException e) {
          e.printStackTrace();
        }
      }
      propSupport.firePropertyChange(INCOMING_MSG, null, msg);
    }
  }

  @Override
  public void handleMessage(String msg) {
    handleMessageFromServer(msg);
  }

  protected void updateRooms(Node module) {
    final Node[] roomNodes = module.getChildren();
    final NodeRoom[] rooms = new NodeRoom[roomNodes.length];
    int defaultRoomIndex = -1;
    for (int i = 0; i < roomNodes.length; ++i) {
      final Node[] playerNodes = roomNodes[i].getChildren();
      final NodePlayer[] players = new NodePlayer[playerNodes.length];
      boolean containsMe = false;
      for (int j = 0; j < playerNodes.length; ++j) {
        players[j] = new NodePlayer(playerNodes[j].getId());
        if (players[j].equals(me)) {
          containsMe = true;
        }
        try {
          final Properties p = new PropertiesEncoder(playerNodes[j].getInfo())
              .getProperties();
          players[j].setInfo(p);
          if (players[j].equals(me)) {
            me.setInfo(p);
          }
        }
        // FIXME: review error message
        catch (final IOException e) {
          e.printStackTrace();
        }
      }

      rooms[i] = new NodeRoom(roomNodes[i].getId(), players);

      // Lock room to start with. The ROOM_INFO message will unlock
      // any rooms that are not locked. Prevents unwanted clients from
      // connecting while room is in an undefined state.
      if (!rooms[i].getName().equals(defaultRoomName)) {
        rooms[i].lock();
      }

      try {
        if (roomNodes[i].getInfo() != null) {
          rooms[i].setInfo(new PropertiesEncoder(roomNodes[i].getInfo())
              .getProperties());
        }
      }
      // FIXME: review error message
      catch (final IOException e) {
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
      allRooms = ArrayUtils.addFirst(rooms, new NodeRoom(defaultRoomName));
    }
    else {
      allRooms = rooms;
      final NodeRoom swap = allRooms[0];
      allRooms[0] = allRooms[defaultRoomIndex];
      allRooms[defaultRoomIndex] = swap;
    }
    // Do not fire a PropertyChange request, The server will be following
    // immediately
    // with a Room List refresh which can cause Icons to flash unexpectedly.
    // propSupport.firePropertyChange(ROOM, null, currentRoom);
    // propSupport.firePropertyChange(AVAILABLE_ROOMS, null, allRooms);
  }

  @Override
  public Player stringToPlayer(String s) {
    NodePlayer p = null;
    try {
      final PropertiesEncoder propEncoder = new PropertiesEncoder(s);
      p = new NodePlayer(null);
      p.setInfo(propEncoder.getProperties());
    }
    // FIXME: review error message
    catch (final IOException e) {
      e.printStackTrace();
    }
    return p;
  }

  @Override
  public String playerToString(Player p) {
    final Properties props = ((NodePlayer) p).toProperties();
    return new PropertiesEncoder(props).getStringValue();
  }

  @Override
  public void initializeControls(ChatServerControls controls) {
    playerStatusControls.initializeControls(controls);
    roomControls.initializeControls(controls);
    controls.setRoomControlsVisible(true);
    final GameModule g = GameModule.getGameModule();
    g.addCommandEncoder(synchEncoder);
    g.addCommandEncoder(privateChatEncoder);
    g.addCommandEncoder(soundEncoder);
    g.addCommandEncoder(inviteEncoder);
    me.setName((String) g.getPrefs().getValue(GameModule.REAL_NAME));
    g.getPrefs().getOption(GameModule.REAL_NAME).addPropertyChangeListener(
        nameChangeListener);
    SimpleStatus s = (SimpleStatus) me.getStatus();
    s = new SimpleStatus(s.isLooking(), s.isAway(), (String) g.getPrefs()
        .getValue(GameModule.PERSONAL_INFO), Info.getVersion(), s.getIp(), g
        .getGameVersion()
        + ((g.getArchiveWriter() == null) ? "" : " " + Resources.getString("Editor.NodeClient.editing")), Long
        .toHexString(g.getCrc()));
    me.setStatus(s);
    g.getPrefs().getOption(GameModule.PERSONAL_INFO).addPropertyChangeListener(
        profileChangeListener);
    controls.getRoomTree().setCellRenderer(new LockableRoomTreeRenderer());
  }

  @Override
  public void uninitializeControls(ChatServerControls controls) {
    roomControls.uninitializeControls(controls);
    playerStatusControls.uninitializeControls(controls);
    final GameModule g = GameModule.getGameModule();
    g.removeCommandEncoder(synchEncoder);
    g.removeCommandEncoder(privateChatEncoder);
    g.removeCommandEncoder(soundEncoder);
    g.removeCommandEncoder(inviteEncoder);
    g.getPrefs().getOption(GameModule.REAL_NAME)
                .removePropertyChangeListener(nameChangeListener);
    g.getPrefs().getOption(GameModule.PERSONAL_INFO)
                .removePropertyChangeListener(profileChangeListener);
  }
}
