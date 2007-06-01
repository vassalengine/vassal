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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import org.jivesoftware.smack.Chat;
import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.muc.HostedRoom;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.ParticipantStatusListener;
import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;

public class JabberClient implements ChatServerConnection, PacketListener, ServerStatus {
  private MessageBoard msgSvr;
  private XMPPConnection conn;
//  private String username;
//  private String password;
  private String host;
  private int port = 5222;
  private PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  private JabberPlayer me = new JabberPlayer("Rodney Kinney" + System.currentTimeMillis(), null);
  private String conferenceService;
  private MonitorRooms monitor;
  private CommandEncoder encoder;
  private final JabberRoom defaultRoom;
  private MultiUserChat currentChat;

  public JabberClient(CommandEncoder encoder, String host, int port, String username, String password) {
    this.host = host;
    this.conferenceService = "conference." + host;
//    this.username = username;
//    this.password = password;
    this.encoder = encoder;
    defaultRoom = JabberRoom.createLocal(this, "Main Room");
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
          conn.close();
        }
        try {
          ConnectionConfiguration config = new ConnectionConfiguration(host, port);
          config.setTLSEnabled(false);
          config.setCompressionEnabled(true);
          config.setSASLAuthenticationEnabled(false);
          config.setDebuggerEnabled(XMPPConnection.DEBUG_ENABLED);
          conn = new XMPPConnection(config);
          // conn.login(username, password);
          conn.loginAnonymously();
          me = new JabberPlayer(me.getName(), conn.getUser());
          // RoomCreationListener roomCreationListener = new
          // RoomCreationListener();
          // conn.addPacketListener(roomCreationListener, roomCreationListener);
          monitor = new MonitorRooms();
          setRoom(defaultRoom);
          propSupport.firePropertyChange(CONNECTED, null, Boolean.TRUE);
          // poll = new Timer();
          // poll.scheduleAtFixedRate(new PollRoomsThread(),1000,30000);
        }
        catch (XMPPException e) {
          reportXMPPException(e);
          setConnected(false);
        }
      }
    }
    else if (isConnected()) {
      leaveCurrentRoom();
      if (monitor != null) {
        monitor.disconnect();
      }
      conn.close();
      conn = null;
      monitor = null;
      propSupport.firePropertyChange(CONNECTED, null, Boolean.FALSE);
    }
  }

  private void leaveCurrentRoom() {
    if (currentChat != null) {
      currentChat.leave();
      currentChat.removeMessageListener(this);
    }
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
    leaveCurrentRoom();
    String oldRoom = getCurrentRoomJID();
    JabberRoom newRoom = null;
    try {
      if (r instanceof JabberRoom) {
        newRoom = (JabberRoom) r;
      }
      else {
        newRoom = JabberRoom.createLocal(this, r.getName());
      }
      currentChat = newRoom.join(this, me);
      monitor.sendRoomChanged(oldRoom, newRoom.getJID());
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
  }

  public Player getUserInfo() {
    return me;
  }

  public void setUserInfo(Player p) {
    // TODO Auto-generated method stub
  }

  public void sendTo(Player recipient, Command c) {
    Chat chat = conn.createChat(((JabberPlayer) recipient).getJid());
    try {
      chat.sendMessage(encoder.encode(c));
    }
    catch (XMPPException e) {
      reportXMPPException(e);
    }
  }

  private void reportXMPPException(XMPPException e) {
    e.printStackTrace();
    if (GameModule.getGameModule() != null) {
      GameModule.getGameModule().warn(e.getMessage());
    }
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

  public static String escapeNode(String node) {
    if (node == null) {
      return null;
    }
    StringBuffer buf = new StringBuffer(node.length() + 8);
    for (int i = 0, n = node.length(); i < n; i++) {
      char c = node.charAt(i);
      switch (c) {
      case '"':
        buf.append("\\22");
        break;
      case '&':
        buf.append("\\26");
        break;
      case '\'':
        buf.append("\\27");
        break;
      case '/':
        buf.append("\\2f");
        break;
      case ':':
        buf.append("\\3a");
        break;
      case '<':
        buf.append("\\3c");
        break;
      case '>':
        buf.append("\\3e");
        break;
      case '@':
        buf.append("\\40");
        break;
      case '\\':
        buf.append("\\5c");
        break;
      default: {
        if (Character.isWhitespace(c)) {
          buf.append("\\20");
        }
        else {
          buf.append(c);
        }
      }
      }
    }
    return buf.toString();
  }

  public static String unescapeNode(String node) {
    if (node == null) {
      return null;
    }
    char[] nodeChars = node.toCharArray();
    StringBuffer buf = new StringBuffer(nodeChars.length);
    for (int i = 0, n = nodeChars.length; i < n; i++) {
      compare: {
        char c = node.charAt(i);
        if (c == '\\' && i + 2 < n) {
          char c2 = nodeChars[i + 1];
          char c3 = nodeChars[i + 2];
          if (c2 == '2') {
            switch (c3) {
            case '0':
              buf.append(' ');
              i += 2;
              break compare;
            case '2':
              buf.append('"');
              i += 2;
              break compare;
            case '6':
              buf.append('&');
              i += 2;
              break compare;
            case '7':
              buf.append('\'');
              i += 2;
              break compare;
            case 'f':
              buf.append('/');
              i += 2;
              break compare;
            }
          }
          else if (c2 == '3') {
            switch (c3) {
            case 'a':
              buf.append(':');
              i += 2;
              break compare;
            case 'c':
              buf.append('<');
              i += 2;
              break compare;
            case 'e':
              buf.append('>');
              i += 2;
              break compare;
            }
          }
          else if (c2 == '4') {
            if (c3 == '0') {
              buf.append("@");
              i += 2;
              break compare;
            }
          }
          else if (c2 == '5') {
            if (c3 == 'c') {
              buf.append("\\");
              i += 2;
              break compare;
            }
          }
        }
        buf.append(c);
      }
    }
    return buf.toString();
  }

  private class MonitorRooms implements PacketListener,
                                        ParticipantStatusListener {
    private static final String PLAYER_CHANGED_ROOMS = "PLAYER_CHANGED_ROOMS";
    private static final String NEW_ROOM = "newRoom";
    private static final String OLD_ROOM = "oldRoom";
    private MultiUserChat monitorRoom;
    private HashMap<String,String> playerToRoom = new HashMap<String,String>();
    private HashMap<String,JabberRoom> jidToRoom =
      new HashMap<String,JabberRoom>();
    private HashMap<String,JabberPlayer> jidToPlayer =
      new HashMap<String,JabberPlayer>();

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
      this.monitorRoom = new MultiUserChat(conn, escapeNode(getModule()) + "@" + getConferenceService());
      // monitorRoom.addParticipantListener(this);
      // monitorRoom.create(getNickName(currentRoom));
      monitorRoom.join(me.getName());
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
        updateRooms(jid);
      }
      monitorRoom.addMessageListener(this);
      monitorRoom.addParticipantStatusListener(this);
    }

    public Room[] getAvailableRooms() {
      ArrayList<Room> rooms = new ArrayList<Room>();
      HashSet<String> s = new HashSet<String>(playerToRoom.values());
      for (String jid : s) {
        Room room = jidToRoom.get(jid);
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
      return jidToRoom.get(jid);
    }

    public void sendRoomChanged(String oldRoom, String newRoom) throws XMPPException {
      Message m = monitorRoom.createMessage();
      if (oldRoom != null) {
        m.setProperty(OLD_ROOM, oldRoom);
      }
      if (newRoom != null) {
        m.setProperty(NEW_ROOM, newRoom);
      }
      m.setBody(PLAYER_CHANGED_ROOMS);
      monitorRoom.sendMessage(m);
      // Don't know why trying this messses things up so badly.
      // updateRooms(monitorRoom.getRoom()+"/"+monitorRoom.getNickname());
    }

    public void disconnect() {
      monitorRoom.leave();
    }

    public void updateRooms(String jid) throws XMPPException {
      removeFromCurrentRoom(jid);
      String newRoomJID = null;
      for (Iterator iter = MultiUserChat.getJoinedRooms(conn, jid); iter.hasNext();) {
        String roomJID = (String) iter.next();
        if (!roomJID.equals(monitorRoom.getRoom())) {
          newRoomJID = roomJID;
        }
      }
      if (newRoomJID != null) {
        addToRoom(jid, newRoomJID);
      }
    }

    private void addToRoom(String jid, String newRoomJID) throws XMPPException {
      JabberPlayer p = getPlayer(jid);
      JabberRoom newRoom = getRoom(newRoomJID);
      newRoom.addPlayer(p);
      playerToRoom.put(jid, newRoomJID);
    }

    private JabberRoom getRoom(String jid) throws XMPPException {
      JabberRoom newRoom = jidToRoom.get(jid);
      if (newRoom == null) {
        newRoom = JabberRoom.createFromJID(JabberClient.this, jid);
        jidToRoom.put(jid, newRoom);
      }
      return newRoom;
    }

    public void processPacket(Packet packet) {
      Message m = (Message) packet;
      if (m.getProperty(NEW_ROOM) != null) {
        try {
          processRoomChange(packet.getFrom(), (String) m.getProperty(NEW_ROOM));
          fireRoomsUpdated();
          updateCurrentRoom((String) m.getProperty(NEW_ROOM));
          updateCurrentRoom((String) m.getProperty(OLD_ROOM));
        }
        catch (XMPPException e) {
          reportXMPPException(e);
        }
      }
    }

    public void joined(String participant) {
    }

    public void left(String participant) {
      String oldRoomJID = playerToRoom.get(participant);
      removeFromCurrentRoom(participant);
      fireRoomsUpdated();
      updateCurrentRoom(oldRoomJID);
    }

    private void updateCurrentRoom(String changedRoomJID) {
      String currentRoom = getCurrentRoomJID();
      if (currentRoom != null && currentRoom.equals(changedRoomJID)) {
        propSupport.firePropertyChange(ROOM, null, getCurrentRoom());
      }
    }

    private void processRoomChange(String jid, String newRoomJID) throws XMPPException {
      removeFromCurrentRoom(jid);
      if (newRoomJID != null) {
        addToRoom(jid, newRoomJID);
      }
      updateCurrentRoom(newRoomJID);
    }

    private void removeFromCurrentRoom(String participant) {
      JabberPlayer p = getPlayer(participant);
      String oldRoomJID = playerToRoom.remove(participant);
      if (oldRoomJID != null) {
        JabberRoom oldRoom = jidToRoom.get(oldRoomJID);
        if (oldRoom != null) {
          oldRoom.removePlayer(p);
          if (oldRoom.getPlayerList().size() == 0) {
            jidToRoom.remove(oldRoomJID);
          }
        }
      }
    }

    private JabberPlayer getPlayer(String jid) {
      JabberPlayer p = jidToPlayer.remove(jid);
      if (p == null) {
        p = new JabberPlayer(StringUtils.parseResource(jid), jid);
        jidToPlayer.put(jid, p);
      }
      return p;
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
      for (Iterator iter = MultiUserChat.getHostedRooms(
        conn, conferenceService).iterator(); iter.hasNext(); ) {

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
}
