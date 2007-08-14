/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.swing.JFrame;

import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.PacketCollector;
import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.SmackConfiguration;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.filter.AndFilter;
import org.jivesoftware.smack.filter.IQTypeFilter;
import org.jivesoftware.smack.filter.PacketFilter;
import org.jivesoftware.smack.filter.PacketIDFilter;
import org.jivesoftware.smack.packet.IQ;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.muc.HostedRoom;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.ParticipantStatusListener;
import org.jivesoftware.smackx.packet.DiscoverItems;
import org.jivesoftware.smackx.packet.VCard;

import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.PrivateChatManager;
import VASSAL.chat.Room;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.SimpleStatus;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.ui.ChatControlsInitializer;
import VASSAL.chat.ui.ChatServerControls;
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

public class JabberClient implements ChatServerConnection, PacketListener, ServerStatus, ChatControlsInitializer {
  private MessageBoard msgSvr;
  private XMPPConnection conn;
  private String username;
  private String password;
  private String host;
  private int port = 5222;
  private PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  private JabberPlayer me;
  private String conferenceService;
  private MonitorRooms monitor;
  private CommandEncoder encoder;
  private final JabberRoom defaultRoom;
  private MultiUserChat currentChat;
  protected MessageBoardControlsInitializer messageBoardControls;
  protected RoomInteractionControlsInitializer roomControls;
  // protected ServerStatusControlsInitializer serverStatusControls;
  protected SimpleStatusControlsInitializer playerStatusControls;

  public JabberClient(CommandEncoder encoder, String host, int port, String username, String password) {
    this.host = host;
    this.conferenceService = "conference." + host;
    this.username = username;
    this.password = password;
    this.encoder = encoder;
    defaultRoom = JabberRoom.getRoomByName(this, "Main Room");
    messageBoardControls = new MessageBoardControlsInitializer(Resources.getString("Chat.messages"), msgSvr); //$NON-NLS-1$
    roomControls = new RoomInteractionControlsInitializer(this);
    roomControls.addPlayerActionFactory(ShowProfileAction.factory());
    roomControls.addPlayerActionFactory(SynchAction.factory(this));
    roomControls.addPlayerActionFactory(PrivateMessageAction.factory(this, new PrivateChatManager(this)));
    roomControls.addPlayerActionFactory(SendSoundAction.factory(this, Resources.getString("Chat.send_wakeup"), "wakeUpSound", "phone1.wav")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    // serverStatusControls = new ServerStatusControlsInitializer(serverStatus);
    playerStatusControls = new SimpleStatusControlsInitializer(this);
    me = JabberPlayer.getPlayerByName(this, username);
  }

  public void addPropertyChangeListener(String propertyName, PropertyChangeListener l) {
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
          ConnectionConfiguration config = new ConnectionConfiguration(host, port);
          config.setSecurityMode(ConnectionConfiguration.SecurityMode.disabled);
          config.setCompressionEnabled(true);
          config.setSASLAuthenticationEnabled(false);
          config.setDebuggerEnabled(XMPPConnection.DEBUG_ENABLED);
          conn = new XMPPConnection(config);
          conn.connect();
          conn.addConnectionListener(new ConnectionListener());
          if (username == null || password == null) {
            conn.loginAnonymously();
          }
          else {
            try {
              conn.login(username, password, "VASSAL");
            }
            catch (XMPPException e) {
              // Create the account if it doesn't exist
              if (e.getXMPPError().getCode() == 401) {
                Map<String, String> attributes = new HashMap<String, String>();
                attributes.put("name", me.getName());
                conn.getAccountManager().createAccount(username, password, attributes);
                conn.login(username, password);
                VCard c = new VCard();
                c.setNickName(me.getName());
                c.save(conn);
              }
              else {
                throw e;
              }
            }
          }
          monitor = new MonitorRooms();
          propSupport.firePropertyChange(CONNECTED, null, Boolean.TRUE);
          setRoom(defaultRoom);
          // poll = new Timer();
          // poll.scheduleAtFixedRate(new PollRoomsThread(),1000,30000);
        }
        catch (XMPPException e) {
          reportXMPPException(e);
          if (e.getWrappedThrowable() != null && e.getWrappedThrowable().getLocalizedMessage() != null) {
            fireStatus(e.getWrappedThrowable().getMessage());
          }
          setConnected(false);
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
    // GameModule.getGameModule().addCommandEncoder(synchEncoder);
    // GameModule.getGameModule().addCommandEncoder(privateChatEncoder);
    // GameModule.getGameModule().addCommandEncoder(soundEncoder);
    // me.setName((String) GameModule.getGameModule().getPrefs().getValue(GameModule.REAL_NAME));
    // GameModule.getGameModule().getPrefs().getOption(GameModule.REAL_NAME).addPropertyChangeListener(nameChangeListener);
    SimpleStatus s = (SimpleStatus) me.getStatus();
    // s = new SimpleStatus(s.isLooking(), s.isAway(), (String)
    // GameModule.getGameModule().getPrefs().getValue(GameModule.PERSONAL_INFO));
    me.setStatus(s);
    // GameModule.getGameModule().getPrefs().getOption(GameModule.PERSONAL_INFO).addPropertyChangeListener(profileChangeListener);
    controls.getRoomTree().setCellRenderer(new LockableRoomTreeRenderer());
  }

  public void uninitializeControls(ChatServerControls controls) {
    messageBoardControls.uninitializeControls(controls);
    roomControls.uninitializeControls(controls);
    // serverStatusControls.uninitializeControls(controls);
    playerStatusControls.uninitializeControls(controls);
    // GameModule.getGameModule().removeCommandEncoder(synchEncoder);
    // GameModule.getGameModule().removeCommandEncoder(privateChatEncoder);
    // GameModule.getGameModule().removeCommandEncoder(soundEncoder);
    // GameModule.getGameModule().getPrefs().getOption(GameModule.REAL_NAME).removePropertyChangeListener(nameChangeListener);
    // GameModule.getGameModule().getPrefs().getOption(GameModule.PERSONAL_INFO).removePropertyChangeListener(profileChangeListener);
  }

  public void processPacket(Packet packet) {
    Message m = (Message) packet;
    if (!m.getFrom().equals(currentChat.getRoom() + "/" + currentChat.getNickname())) {
      propSupport.firePropertyChange(INCOMING_MSG, null, m.getBody());
    }
  }

  public Room getRoom() {
    return monitor.getCurrentRoom();
  }

  public String getCurrentRoomJID() {
    return currentChat == null ? null : currentChat.getRoom();
  }

  public void setRoom(Room r) {
    JabberRoom newRoom = null;
    try {
      if (r instanceof JabberRoom) {
        newRoom = (JabberRoom) r;
      }
      else {
        newRoom = JabberRoom.getRoomByName(this, r.getName());
      }
      if (!newRoom.equals(getRoom())) {
        leaveCurrentRoom();
        currentChat = newRoom.join(this, me);
        me.join(newRoom);
        monitor.sendRoomChanged();
        propSupport.firePropertyChange(ROOM, null, getRoom());
      }
    }
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
    return me;
  }

  public void setUserInfo(Player p) {
    // TODO Auto-generated method stub
  }

  public void sendTo(Player recipient, Command c) {
    Chat chat = conn.getChatManager().createChat(((JabberPlayer) recipient).getJid(), null);
    try {
      chat.sendMessage(encoder.encode(c));
    }
    catch (XMPPException e) {
      reportXMPPException(e);
    }
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
    return "JabberTestModule";
  }

  public String getConferenceService() {
    return conferenceService;
  }

  public static String unescapeNode(String node) {
    return StringUtils.unescapeNode(node);
  }
  /**
   * VASSAL clients join a common room, named for the module, from which they communicate information about which
   * players have joined which rooms, etc.
   * 
   * @author rodneykinney
   * 
   */
  private class MonitorRooms implements PacketListener, ParticipantStatusListener {
    private static final String ROOM_CHANGE_ACTION = "changedRoom";
    private Set<JabberPlayer> players = new HashSet<JabberPlayer>();
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

    public MonitorRooms() throws XMPPException {
      super();
      this.monitorRoom = new MultiUserChat(conn, StringUtils.escapeNode(getModule()) + "@" + getConferenceService());
      // monitorRoom.addParticipantListener(this);
      // monitorRoom.create(getNickName(currentRoom));
      monitorRoom.join(StringUtils.parseName(conn.getUser()));
      try {
        // This is necessary to create the room if it doesn't already exist
        monitorRoom.sendConfigurationForm(new Form(Form.TYPE_SUBMIT));
      }
      catch (XMPPException ex) {
        // 403 code means the room already exists and user is not an owner
        if (ex.getXMPPError().getCode() != 403) {
          throw ex;
        }
      }
      for (Iterator it = monitorRoom.getOccupants(); it.hasNext();) {
        String jid = (String) it.next();
        JabberPlayer player = JabberPlayer.getPlayer(getAbsolutePlayerJID(jid));
        players.add(player);
        updateRooms(player);
      }
      monitorRoom.addMessageListener(this);
      monitorRoom.addParticipantStatusListener(this);
    }

    public Room[] getAvailableRooms() {
      Set<Room> rooms = new HashSet<Room>();
      for (JabberPlayer p : players) {
        JabberRoom room = p.getJoinedRoom();
        if (room != null) {
          rooms.add(room);
        }
      }
      if (!rooms.contains(defaultRoom)) {
        rooms.add(defaultRoom);
      }
      Room[] roomArray = rooms.toArray(new Room[rooms.size()]);
      Arrays.sort(roomArray, roomSortOrder);
      return roomArray;
    }

    public JabberRoom getCurrentRoom() {
      String jid = getCurrentRoomJID();
      return JabberRoom.getRoomByJID(JabberClient.this, jid);
    }

    public void sendRoomChanged() throws XMPPException {
      Message m = monitorRoom.createMessage();
      m.setBody(ROOM_CHANGE_ACTION);
      monitorRoom.sendMessage(m);
    }

    public void disconnect() {
      monitorRoom.leave();
    }

    public void updateRooms(JabberPlayer player) throws XMPPException {
      if (!player.equals(me)) {
        for (Iterator iter = MultiUserChat.getJoinedRooms(conn, player.getJid()); iter.hasNext();) {
          // for (Iterator iter = getJoinedRooms(conn, player.getJid()); iter.hasNext();) {
          String roomJID = (String) iter.next();
          if (!roomJID.equals(monitorRoom.getRoom())) {
            player.join(JabberRoom.getRoomByJID(JabberClient.this, roomJID));
          }
        }
      }
      // VCard c = new VCard();
      // c.load(conn, absJID);
    }

    /**
     * Take the room-local JID for a player (room@conference.server/nick) and change it into an absolute address for
     * that player (login@server/VASSAL)
     * 
     * @param jid
     * @return
     */
    public String getAbsolutePlayerJID(String jid) {
      return StringUtils.parseResource(jid) + "@" + host + "/VASSAL";
    }

    // An attempt at fixing behavior in which MultiUserChat.getJoinedRooms returns empty if a player calls it on himself
    private Iterator<String> getJoinedRooms(XMPPConnection conn, String jid) {
      try {
        ArrayList<String> answer = new ArrayList<String>();
        // Send the disco packet to the user
        String node = "http://jabber.org/protocol/muc#rooms";
        DiscoverItems result;
        // = ServiceDiscoveryManager.getInstanceFor(conn).discoverItems(jid, node);
        // Discover the entity's items
        String entityID = jid;
        DiscoverItems disco = new DiscoverItems();
        disco.setType(IQ.Type.GET);
        disco.setTo(entityID);
        disco.setNode(node);
        // Create a packet collector to listen for a response.
        PacketFilter filter = new AndFilter(new PacketIDFilter(disco.getPacketID()), new IQTypeFilter(IQ.Type.RESULT)) {
          @Override
          public boolean accept(Packet packet) {
            // TODO Auto-generated method stub
            return super.accept(packet);
          }
        };
        // PacketFilter filter = new PacketIDFilter(disco.getPacketID());
        PacketCollector collector = conn.createPacketCollector(filter);
        conn.sendPacket(disco);
        // Wait up to 5 seconds for a result.
        IQ iq = (IQ) collector.nextResult(SmackConfiguration.getPacketReplyTimeout());
        // Stop queuing results
        collector.cancel();
        if (iq == null) {
          System.out.println(jid + " got no response");
          throw new XMPPException("No response from the server.");
        }
        if (iq.getType() == IQ.Type.ERROR) {
          throw new XMPPException(iq.getError());
        }
        result = (DiscoverItems) iq;
        // Collect the entityID for each returned item
        for (Iterator<DiscoverItems.Item> items = result.getItems(); items.hasNext();) {
          String roomJID = items.next().getEntityID();
          System.out.println(jid + " belongs to " + roomJID);
          answer.add(roomJID);
        }
        return answer.iterator();
      }
      catch (XMPPException e) {
        e.printStackTrace();
        // Return an iterator on an empty collection
        return new ArrayList<String>().iterator();
      }
    }

    public void processPacket(Packet packet) {
      Message m = (Message) packet;
      if (ROOM_CHANGE_ACTION.equals(m.getBody())) {
        try {
          JabberPlayer player = JabberPlayer.getPlayer(getAbsolutePlayerJID(packet.getFrom()));
          boolean myRoomChanged = getCurrentRoom().equals(player.getJoinedRoom());
          updateRooms(player);
          fireRoomsUpdated();
          if (getCurrentRoom().equals(player.getJoinedRoom())) {
            myRoomChanged = true;
          }
          if (myRoomChanged) {
            propSupport.firePropertyChange(ROOM, null, getCurrentRoom());
          }
        }
        catch (XMPPException e) {
          reportXMPPException(e);
        }
      }
    }

    public void joined(String participant) {
      players.add(JabberPlayer.getPlayer(getAbsolutePlayerJID(participant)));
    }

    public void left(String participant) {
      String jid = getAbsolutePlayerJID(participant);
      JabberPlayer.deletePlayer(jid);
      JabberPlayer player = JabberPlayer.getPlayer(jid);
      if (getCurrentRoom().equals(player.getJoinedRoom())) {
        propSupport.firePropertyChange(ROOM, null, getCurrentRoom());
      }
      players.remove(player);
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
      // participants.remove(participant);
      // participants.add(newNickname);
      // refreshRooms();
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
      for (Iterator iter = MultiUserChat.getHostedRooms(conn, conferenceService).iterator(); iter.hasNext();) {
        HostedRoom room = (HostedRoom) iter.next();
        MultiUserChat.getRoomInfo(conn, room.getJid());
      }
    }
    catch (XMPPException e) {
      e.printStackTrace();
    }
    return entries.toArray(new ModuleSummary[entries.size()]);
  }

  public String[] getSupportedTimeRanges() {
    return new String[0];
  }
  private class ConnectionListener implements org.jivesoftware.smack.ConnectionListener {
    public void connectionClosed() {
    }

    public void connectionClosedOnError(Exception e) {
      String msg = e.getMessage();
      if (e instanceof XMPPException) {
        XMPPException xe = (XMPPException) e;
        if (xe.getStreamError() != null && "conflict".equals(xe.getStreamError().getCode())) {
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
    // XMPPConnection.DEBUG_ENABLED = true;
    CommandEncoder c = new CommandEncoder() {
      public Command decode(String command) {
        System.err.println(command);
        return null;
      }

      public String encode(Command c) {
        return null;
      }
    };
    String username = args.length == 0 ? "test" : args[0];
    String password = args.length == 0 ? "test" : args[1];
    JabberClient client = new JabberClient(c, "63.144.41.3", 5222, username, password);
    client.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        System.err.println(evt.getPropertyName() + "=" + evt.getNewValue());
      }
    });
    ChatServerControls controls = new ChatServerControls();
    controls.setClient(client);
    JFrame f = new JFrame(username);
    f.getContentPane().add(controls.getControls());
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
}
