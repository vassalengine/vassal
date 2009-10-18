/*
 * $Id$
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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
package VASSAL.chat.jabber;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.filter.AndFilter;
import org.jivesoftware.smack.filter.FromContainsFilter;
import org.jivesoftware.smack.filter.IQTypeFilter;
import org.jivesoftware.smack.filter.PacketFilter;
import org.jivesoftware.smack.filter.PacketTypeFilter;
import org.jivesoftware.smack.packet.IQ;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.muc.HostedRoom;
import org.jivesoftware.smackx.muc.InvitationListener;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.ParticipantStatusListener;
import org.jivesoftware.smackx.packet.DiscoverItems;
import org.jivesoftware.smackx.packet.VCard;

import VASSAL.build.GameModule;
import VASSAL.chat.LockableChatServerConnection;
import VASSAL.chat.LockableRoom;
import VASSAL.chat.Player;
import VASSAL.chat.PlayerEncoder;
import VASSAL.chat.PrivateChatManager;
import VASSAL.chat.Room;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleStatus;
import VASSAL.chat.SynchEncoder;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.ui.ChatControlsInitializer;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.chat.ui.InviteAction;
import VASSAL.chat.ui.KickAction;
import VASSAL.chat.ui.LockableRoomTreeRenderer;
import VASSAL.chat.ui.MessageBoardControlsInitializer;
import VASSAL.chat.ui.PrivateMessageAction;
import VASSAL.chat.ui.RoomInteractionControlsInitializer;
import VASSAL.chat.ui.SendSoundAction;
import VASSAL.chat.ui.ShowProfileAction;
import VASSAL.chat.ui.SimpleStatusControlsInitializer;
import VASSAL.chat.ui.SynchAction;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.Dialogs;

public class JabberClient implements LockableChatServerConnection,
    PacketListener, ServerStatus, ChatControlsInitializer, PlayerEncoder {
  private static final String QUERY_ROOMS = "http://jabber.org/protocol/muc#rooms";
  private MessageBoard msgSvr;
  private XMPPConnection conn;
  private String host;
  private int port = 5222;
  private PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  private JabberPlayer me;
  private String conferenceService;
  private MonitorRooms monitor;
  private CommandEncoder encoder;
  private final JabberRoom defaultRoom;
  private MultiUserChat currentChat;
  private AccountInfo account;
  private SynchEncoder synchEncoder;
  protected MessageBoardControlsInitializer messageBoardControls;
  protected RoomInteractionControlsInitializer roomControls;
  // protected ServerStatusControlsInitializer serverStatusControls;
  protected SimpleStatusControlsInitializer playerStatusControls;
  protected JabberPlayer.Manager playerMgr = new JabberPlayer.Manager();
  protected JabberRoom.Manager roomMgr = new JabberRoom.Manager();
  protected PropertyChangeListener idChangeListener;

  public JabberClient(CommandEncoder encoder, String host, int port,
      AccountInfo account) {
    XMPPConnection.DEBUG_ENABLED = "true".equals(System
        .getProperty("enableJabber"));
    this.host = host;
    this.conferenceService = "conference." + host;
    this.encoder = encoder;
    this.account = account;
    defaultRoom = roomMgr.getRoomByName(this, "Main Room");
    messageBoardControls = new MessageBoardControlsInitializer(Resources
        .getString("Chat.messages"), msgSvr); //$NON-NLS-1$
    // roomControls = new RoomInteractionControlsInitializer(this);
    roomControls = new LockableJabberRoomControls(this);
    roomControls.addPlayerActionFactory(ShowProfileAction.factory());
    roomControls.addPlayerActionFactory(SynchAction.factory(this));
    roomControls.addPlayerActionFactory(PrivateMessageAction.factory(this,
        new PrivateChatManager(this)));
    roomControls.addPlayerActionFactory(SendSoundAction.factory(this, Resources
        .getString("Chat.send_wakeup"), "wakeUpSound", "phone1.wav")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    roomControls.addPlayerActionFactory(InviteAction.factory(this));
    roomControls.addPlayerActionFactory(KickAction.factory(this));
    // serverStatusControls = new ServerStatusControlsInitializer(serverStatus);
    playerStatusControls = new SimpleStatusControlsInitializer(this);
    synchEncoder = new SynchEncoder(this, this);

    idChangeListener = new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        if (me != null) {
          SimpleStatus s = (SimpleStatus) me.getStatus();
          s.updateStatus();
          me.setStatus(s);
          me.setName((String) GameModule.getGameModule().getPrefs().getValue(
              GameModule.REAL_NAME));
        }
        if (monitor != null) {
          monitor.sendStatus(me);
        }
      }
    };
  }

  public void addPropertyChangeListener(String propertyName,
      PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(propertyName, l);
  }

  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  public boolean isConnected() {
    return conn != null && conn.isConnected();
  }

  public void sendToOthers(Command c) {
    if (currentChat != null) {
      try {
        currentChat.sendMessage(encoder.encode(c));
      }
      // FIXME: review error message
      catch (XMPPException e) {
        reportXMPPException(e);
      }
    }
  }

  public void setConnected(boolean connect) {
    if (connect) {
      if (!isConnected()) {
        if (conn != null) {
          conn.disconnect();
        }
        try {
          String username = account.getUserName();
          String password = account.getPassword();
          me = playerMgr.getPlayerByLogin(this, account.getUserName());

          final GameModule g = GameModule.getGameModule();
          SimpleStatus s = (SimpleStatus) me.getStatus();
          s.updateStatus();
          me.setStatus(s);

          // me.setName(account.getRealName());
          me.setName((String) g.getPrefs().getValue(GameModule.REAL_NAME));
          ConnectionConfiguration config = new ConnectionConfiguration(host,
              port);
          config.setSecurityMode(ConnectionConfiguration.SecurityMode.disabled);
          config.setCompressionEnabled(true);
          config.setSASLAuthenticationEnabled(false);
          config.setDebuggerEnabled(XMPPConnection.DEBUG_ENABLED);
          conn = new XMPPConnection(config);
          conn.connect();
          conn.addConnectionListener(new ConnectionListener());
          try {
            conn.login(username, password, "VASSAL");
          }
          // FIXME: review error message
          catch (XMPPException e) {
            // Create the account if it doesn't exist
            if (e.getXMPPError() != null && e.getXMPPError().getCode() == 401) {
              Map<String, String> attributes = new HashMap<String, String>();
              attributes.put("name", me.getName());
              try {
                conn.getAccountManager().createAccount(username, password,
                    attributes);
              }
              // FIXME: review error message
              catch (XMPPException createAccountError) {
                if (createAccountError.getXMPPError() != null
                    && createAccountError.getXMPPError().getCode() == 409) {
                  // Account already exists. Password is incorrect
                  fireStatus(Resources.getString("Chat.invalid_password",
                      username));
                  setConnected(false);
                  return;
                }
                else {
                  throw e;
                }
              }
              conn.login(username, password, "VASSAL");
              VCard c = new VCard();
              c.setNickName(me.getName());
              c.save(conn);
            }
            else {
              throw e;
            }
          }
          monitor = new MonitorRooms();
          monitor.init();
          propSupport.firePropertyChange(CONNECTED, null, Boolean.TRUE);
          setRoom(defaultRoom);
          fireStatus(Resources.getString("Server.connected", host + ":" + port));
          GameModule.getGameModule().addIdChangeListener(idChangeListener);
          MultiUserChat.addInvitationListener(conn, new InvitationListener() {
            public void invitationReceived(XMPPConnection conn, String room,
                String inviter, String reason, String password, Message mess) {
              final String playerName = inviter.split("@")[0];
              final String roomName = JabberRoom.jidToName(room);
              final int i = Dialogs.showConfirmDialog(GameModule
                  .getGameModule().getFrame(), "Invitation to join room",
                  "Invitation to join room", Resources.getString(
                      "Chat.invitation", playerName, roomName),
                  JOptionPane.QUESTION_MESSAGE, null,
                  JOptionPane.YES_NO_OPTION, "Invite" + inviter, Resources
                      .getString("Chat.ignore_invitation"));
              if (i == 0) {
                doInvite(inviter, roomName);
              }
              else {
                MultiUserChat.decline(conn, room, inviter, "");                
              }
            }
          });

        }
        // FIXME: review error message
        catch (XMPPException e) {
          reportXMPPException(e);
          fireStatus(Resources.getString("Server.server_error", e
              .getXMPPError().getMessage(), e.getXMPPError().getCondition(), e
              .getXMPPError().getCode()));
          setConnected(false);
          return;
        }
      }
    }
    else {
      if (isConnected()) {
        leaveCurrentRoom();
        if (monitor != null) {
          monitor.disconnect();
        }
        conn.disconnect();
      }
      conn = null;
      monitor = null;
      currentChat = null;
      propSupport.firePropertyChange(CONNECTED, null, Boolean.FALSE);
      playerMgr.clear();
      roomMgr.clear();
      fireStatus(Resources.getString("Server.disconnected", host + ":" + port));
    }
  }

  private void leaveCurrentRoom() {
    if (currentChat != null) {
      currentChat.leave();
      currentChat.removeMessageListener(this);
      currentChat = null;
    }
  }

  public void initializeControls(ChatServerControls controls) {
    playerStatusControls.initializeControls(controls);
    messageBoardControls.initializeControls(controls);
    roomControls.initializeControls(controls);
    // serverStatusControls.initializeControls(controls);
    GameModule.getGameModule().addCommandEncoder(synchEncoder);
    // GameModule.getGameModule().addCommandEncoder(privateChatEncoder);
    // GameModule.getGameModule().addCommandEncoder(soundEncoder);
    controls.getRoomTree().setCellRenderer(new LockableRoomTreeRenderer());
  }

  public void uninitializeControls(ChatServerControls controls) {
    messageBoardControls.uninitializeControls(controls);
    roomControls.uninitializeControls(controls);
    playerStatusControls.uninitializeControls(controls);
    // serverStatusControls.uninitializeControls(controls);
    GameModule.getGameModule().removeCommandEncoder(synchEncoder);
    // GameModule.getGameModule().removeCommandEncoder(privateChatEncoder);
    // GameModule.getGameModule().removeCommandEncoder(soundEncoder);
  }

  public void processPacket(Packet packet) {
    Message m = (Message) packet;
    if (!m.getFrom().equals(
        currentChat.getRoom() + "/" + currentChat.getNickname())) {
      propSupport.firePropertyChange(INCOMING_MSG, null, m.getBody());
    }
  }

  public Room getRoom() {
    return monitor.getCurrentRoom();
  }

  public String getCurrentRoomJID() {
    return currentChat == null ? null : currentChat.getRoom();
  }

  public void setRoom(String roomName) {
    setRoom(roomMgr.getRoomByName(this, roomName));
  }
  
  public void setRoom(Room r) {
    JabberRoom newRoom = null;
    try {
      if (r instanceof JabberRoom) {
        newRoom = (JabberRoom) r;
      }
      else {
        newRoom = roomMgr.getRoomByName(this, r.getName());
      }
      if (!newRoom.equals(getRoom())) {
        leaveCurrentRoom();
        currentChat = newRoom.join(this, (JabberPlayer) getUserInfo());
        monitor.sendRoomChanged();
      }
    }
    // FIXME: review error message
    // 401 = Bad Password for room

    catch (XMPPException e) {
      reportXMPPException(e);
      propSupport.firePropertyChange(STATUS, null, "Failed to join room");
    }
  }

  public Room[] getAvailableRooms() {
    return monitor.getAvailableRooms();
  }

  protected void fireRoomsUpdated() {
    propSupport.firePropertyChange(AVAILABLE_ROOMS, null, getAvailableRooms());
    propSupport.firePropertyChange(ROOM, null, getRoom());
  }

  protected void fireStatus(String msg) {
    propSupport.firePropertyChange(STATUS, null, msg);
  }

  public Player getUserInfo() {
    return playerMgr.getPlayerByLogin(this, account.getUserName());
  }

  public void setUserInfo(Player p) {
    if (monitor != null) {
      monitor.sendStatus((SimplePlayer) p);
    }
  }

  public String getDefaultRoomName() {
    return defaultRoom.getName();
  }

  public void sendTo(Player recipient, Command c) {
    Chat chat = conn.getChatManager().createChat(
        ((JabberPlayer) recipient).getJid(), null);
    try {
      chat.sendMessage(encoder.encode(c));
    }
    // FIXME: review error message
    catch (XMPPException e) {
      reportXMPPException(e);
    }
  }

  /** Can a player be invited to this room? */
  public boolean isInvitable(Player invitee) {
    // invitee is not me
    if (!invitee.equals(me)) {
      // invitee is in a different room
      final JabberRoom room = monitor.getCurrentRoom();
      if (!room.contains(invitee)) {
        // I own the current room and it is locked
        if (room.isOwnedByMe() && room.isLocked()) {
          return true;
        }
      }
    }
    return false;
  }

  /** Send invitation to player */
  public void sendInvite(Player invitee) {
    try {
      currentChat.grantMembership(invitee.getId());
    }
    catch (XMPPException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    currentChat.invite(invitee.getId(), "");
  }

  /** Process an invitation */
  public void doInvite(String playerId, String roomName) {
    setRoom(roomName);
  }

  /** Is a player kickable from this room? */
  public boolean isKickable(Player kickee) {
    // kickee is not me
    if (!kickee.equals(me)) {
      // kickee is in this room
      final JabberRoom room = monitor.getCurrentRoom();
      if (room.contains(kickee)) {
        // I own the current room and it is locked
        if (room.isOwnedByMe() && room.isLocked()) {
          return true;
        }
      }
    }
    return false;
  }

  /** Kick a player from this room */
  public void kick(Player kickee) {

  }

  private void reportXMPPException(XMPPException e) {
    e.printStackTrace();
  }

  public MessageBoard getMessageServer() {
    return msgSvr;
  }

  public ServerStatus getStatusServer() {
    return this;
  }

  public XMPPConnection getConnection() {
    return conn;
  }

  public String getModule() {
    return "vassal/" + GameModule.getGameModule().getGameName();
  }

  public String getConferenceService() {
    return conferenceService;
  }

  public static String unescapeNode(String node) {
    return StringUtils.unescapeNode(node);
  }

  /**
   * Toggle the lock state on the room.
   */
  public void lockRoom(LockableRoom r) {
    if (r instanceof JabberRoom) {
      final JabberRoom room = (JabberRoom) r;
      room.toggleLock(currentChat);
      try {
        monitor.sendRoomChanged();
      }
      catch (XMPPException e) {
        // Ignore errors - we don't want to know at this point
      }
    }
  }

  /**
   * VASSAL clients join a common room, named for the module, from which they
   * communicate information about which players have joined which rooms, etc.
   * 
   * @author rodneykinney
   * 
   */
  private class MonitorRooms implements PacketListener,
      ParticipantStatusListener {
    private static final String ROOM_CHANGE_ACTION = "changedRoom";
    private MultiUserChat monitorRoom;
    private Comparator<Room> roomSortOrder = new Comparator<Room>() {
      public int compare(Room o1, Room o2) {
        if (o1.equals(defaultRoom) && !o2.equals(defaultRoom)) {
          return -1;
        }
        else if (o2.equals(defaultRoom) && !o1.equals(defaultRoom)) {
          return 1;
        }
        else {
          return o1.getName().compareTo(o2.getName());
        }
      }
    };

    public void init() throws XMPPException {
      new TrackRooms().addTo(conn);
      new TrackStatus(getMonitorRoomJID().toLowerCase()).addTo(conn);
      monitorRoom = new MultiUserChat(conn, getMonitorRoomJID());
      monitorRoom.addMessageListener(this);
      monitorRoom.addParticipantStatusListener(this);
      monitorRoom.join(StringUtils.parseName(conn.getUser()));
      try {
        // This is necessary to create the room if it doesn't already exist
        monitorRoom.sendConfigurationForm(new Form(Form.TYPE_SUBMIT));
      }
      catch (XMPPException ex) {
        // 403 code means the room already exists and user is not an owner
        if (ex.getXMPPError() != null && ex.getXMPPError().getCode() != 403) {
          throw ex;
        }
      }
      sendStatus(me);
    }

    public String getMonitorRoomJID() {
      return StringUtils.escapeNode(getModule()) + "@" + getConferenceService();
    }

    public void sendStatus(SimplePlayer p) {
      sendStatus(p, null);
    }

    public void sendStatus(SimplePlayer player, String recipient) {
      SimpleStatus s = (SimpleStatus) player.getStatus();
      Presence p = new Presence(Presence.Type.available);
      p.setStatus("");
      p.setMode(Presence.Mode.chat);
      p.setProperty(SimpleStatus.LOOKING, s.isLooking());
      p.setProperty(SimpleStatus.AWAY, s.isAway());
      p.setProperty(SimpleStatus.IP, s.getIp());
      p.setProperty(SimpleStatus.CLIENT, s.getClient());
      p.setProperty(SimpleStatus.MODULE_VERSION, s.getModuleVersion());
      p.setProperty(SimpleStatus.CRC, s.getCrc());
      p.setProperty("realName", player.getName());
      p.setTo(recipient == null ? monitorRoom.getRoom() : recipient);
      conn.sendPacket(p);
    }

    public Room[] getAvailableRooms() {
      Map<JabberRoom, List<JabberPlayer>> occupants = new HashMap<JabberRoom, List<JabberPlayer>>();
      for (JabberPlayer p : playerMgr.getAllPlayers()) {
        JabberRoom room = p.getJoinedRoom();
        if (room != null) {
          List<JabberPlayer> l = occupants.get(room);
          if (l == null) {
            l = new ArrayList<JabberPlayer>();
            occupants.put(room, l);
          }
          l.add(p);
        }
      }
      if (!occupants.containsKey(defaultRoom)) {
        List<JabberPlayer> l = Collections.emptyList();
        occupants.put(defaultRoom, l);
      }
      Set<JabberRoom> rooms = occupants.keySet();
      for (JabberRoom room : rooms) {
        List<JabberPlayer> l = occupants.get(room);
        room.setPlayers(l.toArray(new JabberPlayer[l.size()]));
      }
      Room[] roomArray = rooms.toArray(new Room[rooms.size()]);
      Arrays.sort(roomArray, roomSortOrder);
      return roomArray;
    }

    public JabberRoom getCurrentRoom() {
      String jid = getCurrentRoomJID();
      return roomMgr.getRoomByJID(JabberClient.this, jid);
    }

    public void sendRoomChanged() throws XMPPException {
      Message m = monitorRoom.createMessage();
      m.setBody(ROOM_CHANGE_ACTION);
      monitorRoom.sendMessage(m);
    }

    public void disconnect() {
      monitorRoom.leave();
    }

    /**
     * Take the room-local JID for a player (room@conference.server/nick) and
     * change it into an absolute address for that player (login@server/VASSAL)
     * 
     * @param jid
     * @return
     */
    public String getAbsolutePlayerJID(String jid) {
      return StringUtils.parseResource(jid) + "@" + host + "/VASSAL";
    }

    private void sendRoomQuery(String jid) {
      System.out.println("Sending room query to " + jid);
      DiscoverItems disco = new DiscoverItems();
      disco.setType(IQ.Type.GET);
      disco.setTo(jid);
      disco.setNode(QUERY_ROOMS);
      conn.sendPacket(disco);
    }

    public void processPacket(Packet packet) {
      Message m = (Message) packet;
      if (ROOM_CHANGE_ACTION.equals(m.getBody())) {
        String jid = getAbsolutePlayerJID(packet.getFrom());
        playerMgr.getPlayer(getAbsolutePlayerJID(packet.getFrom()));
        sendRoomQuery(jid);
      }
    }

    public void joined(String participant) {
      playerMgr.getPlayer(getAbsolutePlayerJID(participant));
    }

    public void left(String participant) {
      String jid = getAbsolutePlayerJID(participant);
      playerMgr.deletePlayer(jid);
      fireRoomsUpdated();
    }

    public void kicked(String participant, String actor, String reason) {
    }

    public void voiceGranted(String participant) {
    }

    public void voiceRevoked(String participant) {
    }

    public void banned(String participant, String actor, String reason) {
    }

    public void membershipGranted(String participant) {
    }

    public void membershipRevoked(String participant) {
    }

    public void moderatorGranted(String participant) {
    }

    public void moderatorRevoked(String participant) {
    }

    public void ownershipGranted(String participant) {
    }

    public void ownershipRevoked(String participant) {
    }

    public void adminGranted(String participant) {
    }

    public void adminRevoked(String participant) {
    }

    public void nicknameChanged(String participant, String newNickname) {
    }

    private class TrackStatus extends PacketProcessor {
      String prefix;

      public TrackStatus(String prefix) {
        this.prefix = prefix;
      }

      public boolean acceptPacket(Packet packet) {
        boolean accept = false;
        if (packet instanceof Presence) {
          Presence p = (Presence) packet;
          if (p.getType() == Presence.Type.available
              && p.getMode() == Presence.Mode.chat) {
            String name = p.getFrom();
            if (name != null && name.toLowerCase().startsWith(prefix)) {
              accept = true;
            }
          }
        }
        return accept;
      }

      public void process(Packet packet) {
        Presence p = (Presence) packet;
        JabberPlayer player = playerMgr.getPlayer(getAbsolutePlayerJID(p
            .getFrom()));
        SimpleStatus status = (SimpleStatus) player.getStatus();
        String profile = status == null ? "" : status.getProfile();
        Object looking = p.getProperty(SimpleStatus.LOOKING);
        Object away = p.getProperty(SimpleStatus.AWAY);
        String ip = p.getProperty(SimpleStatus.IP).toString();
        String client = p.getProperty(SimpleStatus.CLIENT).toString();
        String moduleVersion = p.getProperty(SimpleStatus.MODULE_VERSION)
            .toString();
        String crc = p.getProperty(SimpleStatus.CRC).toString();
        status = new SimpleStatus(looking == null ? false : (Boolean) looking,
            away == null ? false : (Boolean) away, profile, client, ip,
            moduleVersion, crc);
        player.setStatus(status);
        player.setName(String.valueOf(p.getProperty("realName")));
        fireRoomsUpdated();
      }
    }

    private class TrackRooms extends PacketProcessor {
      private PacketFilter roomResponseFilter = new AndFilter(new IQTypeFilter(
          IQ.Type.RESULT), new PacketTypeFilter(DiscoverItems.class));
      private PacketFilter newPlayerFilter = new AndFilter(
          new PacketTypeFilter(Presence.class), new FromContainsFilter(
              getMonitorRoomJID()));

      public TrackRooms() {
      }

      public void process(Packet packet) {
        if (roomResponseFilter.accept(packet)) {
          DiscoverItems result = (DiscoverItems) packet;
          JabberPlayer player = playerMgr.getPlayer(packet.getFrom());
          // Collect the entityID for each returned item
          for (Iterator<DiscoverItems.Item> items = result.getItems(); items
              .hasNext();) {
            String roomJID = items.next().getEntityID();
            JabberRoom room = roomMgr.getRoomByJID(JabberClient.this, roomJID);
            try {
              room.setInfo(MultiUserChat.getRoomInfo(JabberClient.this
                  .getConnection(), roomJID));
            }
            catch (XMPPException e) {
              // Ignore Error
            }
            if (!roomJID.equals(monitorRoom.getRoom())) {
              player.join(roomMgr.getRoomByJID(JabberClient.this, roomJID));
            }
          }
          fireRoomsUpdated();
        }
        else if (newPlayerFilter.accept(packet)) {
          sendRoomQuery(getAbsolutePlayerJID(packet.getFrom()));
        }
      }

      public boolean acceptPacket(Packet packet) {
        boolean accept = false;
        if (roomResponseFilter.accept(packet)) {
          accept = QUERY_ROOMS.equals(((DiscoverItems) packet).getNode());
        }
        else if (newPlayerFilter.accept(packet)) {
          accept = ((Presence) packet).isAvailable();
        }
        return accept;
      }
    }
  }

  public CommandEncoder getEncoder() {
    return encoder;
  }

  public void setEncoder(CommandEncoder encoder) {
    this.encoder = encoder;
  }

  public ModuleSummary[] getHistory(String timeRange) {
    return new ModuleSummary[0];
  }

  public ModuleSummary[] getStatus() {
    ArrayList<ModuleSummary> entries = new ArrayList<ModuleSummary>();
    try {
      for (Iterator<HostedRoom> iter = MultiUserChat.getHostedRooms(conn,
          conferenceService).iterator(); iter.hasNext();) {
        HostedRoom room = iter.next();
        MultiUserChat.getRoomInfo(conn, room.getJid());
      }
    }
    // FIXME: review error message
    catch (XMPPException e) {
      e.printStackTrace();
    }
    return entries.toArray(new ModuleSummary[entries.size()]);
  }

  public String[] getSupportedTimeRanges() {
    return new String[0];
  }

  private class ConnectionListener implements
      org.jivesoftware.smack.ConnectionListener {
    public void connectionClosed() {
    }

    public void connectionClosedOnError(Exception e) {
      String msg = e.getMessage();
      if (e instanceof XMPPException) {
        XMPPException xe = (XMPPException) e;
        if (xe.getStreamError() != null
            && "conflict".equals(xe.getStreamError().getCode())) {
          msg = "Another user has logged in using the same account.  Disconnecting";
        }
      }
      if (msg != null) {
        fireStatus(msg);
      }
      setConnected(false);
    }

    public void reconnectingIn(int seconds) {
    }

    public void reconnectionFailed(Exception e) {
    }

    public void reconnectionSuccessful() {
    }
  }

  public static void main(String[] args) {
    XMPPConnection.DEBUG_ENABLED = true;
    CommandEncoder c = new CommandEncoder() {
      public Command decode(String command) {
        System.err.println(command);
        return null;
      }

      public String encode(Command c) {
        return null;
      }
    };
    final String username = args.length == 0 ? "test" : args[0];
    final String password = args.length == 0 ? "test" : args[1];
    // JabberClient client = new JabberClient(c, "63.144.41.3", 5222, username,
    // password);
    AccountInfo account = new AccountInfo() {
      public String getPassword() {
        return password;
      }

      public String getUserName() {
        return username;
      }

      public String getModule() {
        return "JabberTestModule";
      }

      public String getRealName() {
        return username;
      }
    };
    JabberClient client = new JabberClient(c, "localhost", 5222, account);
    client.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        System.err.println(evt.getPropertyName() + "=" + evt.getNewValue());
      }
    });
    ChatServerControls controls = new ChatServerControls();
    controls.setClient(client);
    JFrame f = new JFrame(username);
    f.add(controls.getControls());
    f.pack();
    f.setVisible(true);
    f.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });
  }

  public String getHost() {
    return host;
  }

  public String playerToString(Player p) {
    return ((JabberPlayer) p).getJid();
  }

  public Player stringToPlayer(String s) {
    return playerMgr.getPlayer(s);
  }

}
