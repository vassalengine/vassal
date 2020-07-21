/*
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
package VASSAL.chat.jabber;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
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
import org.jivesoftware.smack.filter.MessageTypeFilter;
import org.jivesoftware.smack.filter.PacketFilter;
import org.jivesoftware.smack.filter.PacketTypeFilter;
import org.jivesoftware.smack.packet.IQ;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.PacketExtension;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.XMPPError;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.muc.DefaultParticipantStatusListener;
import org.jivesoftware.smackx.muc.DefaultUserStatusListener;
import org.jivesoftware.smackx.muc.HostedRoom;
import org.jivesoftware.smackx.muc.InvitationListener;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.ParticipantStatusListener;
import org.jivesoftware.smackx.muc.UserStatusListener;
import org.jivesoftware.smackx.packet.DiscoverItems;
import org.jivesoftware.smackx.packet.MUCUser;
import org.jivesoftware.smackx.packet.VCard;

import VASSAL.build.GameModule;
import VASSAL.chat.LockableChatServerConnection;
import VASSAL.chat.LockableRoom;
import VASSAL.chat.Player;
import VASSAL.chat.PlayerEncoder;
import VASSAL.chat.PrivateChatEncoder;
import VASSAL.chat.PrivateChatManager;
import VASSAL.chat.Room;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.SimpleStatus;
import VASSAL.chat.SoundEncoder;
import VASSAL.chat.SynchEncoder;
import VASSAL.chat.messageboard.MessageBoard;
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
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.io.DeobfuscatingInputStream;
import VASSAL.tools.io.FastByteArrayOutputStream;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.ObfuscatingOutputStream;
import VASSAL.tools.swing.Dialogs;

public class JabberClient implements LockableChatServerConnection, PacketListener, ServerStatus, ChatControlsInitializer, PlayerEncoder {
  private static final String QUERY_ROOMS = "http://jabber.org/protocol/muc#rooms"; //$NON-NLS-1$
  private static final String QUERY_USER = "http://jabber.org/protocol/muc#user"; //$NON-NLS-1$
  private static final String INVITE = "Invite"; //$NON-NLS-1$
  private static final String REAL_NAME = "realName"; //$NON-NLS-1$
  public static final String JID_RESOURCE = "/VASSAL"; //$NON-NLS-1$
  public static final String ROOM_CONFIG = "roomConfig"; //$NON-NLS-1$
  public static final String ROOM_JID = "roomJid"; //$NON-NLS-1$
  public static final String ROOM_NAME = "roomName"; //$NON-NLS-1$
  public static final String OWNER = "owner"; //$NON-NLS-1$

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
  protected SoundEncoder soundEncoder;
  protected PrivateChatEncoder privateChatEncoder;
  // protected MessageBoardControlsInitializer messageBoardControls;
  protected RoomInteractionControlsInitializer roomControls;
  // protected ServerStatusControlsInitializer serverStatusControls;
  protected SimpleStatusControlsInitializer playerStatusControls;
  protected JabberPlayer.Manager playerMgr = new JabberPlayer.Manager();
  protected JabberRoom.Manager roomMgr = new JabberRoom.Manager();
  protected PropertyChangeListener idChangeListener;
  protected UserStatusListener kickListener;
  protected InvitationListener inviteListener;
  protected ParticipantStatusListener userListener;

  public JabberClient(CommandEncoder encoder, String host, int port, AccountInfo account) {
    XMPPConnection.DEBUG_ENABLED = "true".equals(System.getProperty("debugJabber"));
    this.host = host;
    this.conferenceService = "conference." + host; //$NON-NLS-1$
    this.encoder = encoder;
    this.account = account;
    defaultRoom = roomMgr.getRoomByName(this, DEFAULT_ROOM_NAME);
    // messageBoardControls = new MessageBoardControlsInitializer(Resources.getString("Chat.messages"), msgSvr); //$NON-NLS-1$
    roomControls = new LockableJabberRoomControls(this);
    roomControls.addPlayerActionFactory(ShowProfileAction.factory());
    roomControls.addPlayerActionFactory(SynchAction.factory(this));
    final PrivateChatManager privateChatManager = new PrivateChatManager(this);
    roomControls.addPlayerActionFactory(PrivateMessageAction.factory(this, privateChatManager));
    roomControls.addPlayerActionFactory(SendSoundAction.factory(this, Resources.getString("Chat.send_wakeup"), "wakeUpSound", "phone1.wav")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    roomControls.addPlayerActionFactory(InviteAction.factory(this));
    roomControls.addPlayerActionFactory(KickAction.factory(this));
    // serverStatusControls = new ServerStatusControlsInitializer(serverStatus);
    playerStatusControls = new SimpleStatusControlsInitializer(this);
    synchEncoder = new SynchEncoder(this,this);
    soundEncoder = new SoundEncoder(this);
    privateChatEncoder = new PrivateChatEncoder(this, privateChatManager);

    // Listen for changes to our name via VASSAL preferences
    idChangeListener = new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        if (me != null) {
          final SimpleStatus s = (SimpleStatus) me.getStatus();
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

    // Listen for someone kicking us from the current room
    kickListener = new DefaultUserStatusListener() {
      @Override
      public void kicked(String kicker, String reason) {
        fireStatus(Resources.getString("Chat.kicked", getRoom().getName())); //$NON-NLS-1$
        setRoom(defaultRoom);
      }
    };

    // Listen for someone inviting us to another room
    inviteListener = new InvitationListener() {
      @Override
      public void invitationReceived(XMPPConnection conn, String room,
          String inviter, String reason, String password, Message mess) {
        if (INVITE.equals(reason)) {
          final String playerLogin = inviter.split("@")[0]; //$NON-NLS-1$
          final Player player = playerMgr.getPlayer(inviter+JID_RESOURCE);
          final String playerName = player.getName() + "(" + playerLogin + ")"; //$NON-NLS-1$ //$NON-NLS-2$
          final String roomName = roomMgr.getRoomByJID(JabberClient.this, room).getName();
          final int i = Dialogs.showConfirmDialog(GameModule
            .getGameModule().getFrame(), Resources.getString("Chat.invite_heading"), //$NON-NLS-1$
            Resources.getString("Chat.invite_heading"), Resources.getString( //$NON-NLS-1$
                "Chat.invitation", playerName, roomName), //$NON-NLS-1$
            JOptionPane.QUESTION_MESSAGE, null,
            JOptionPane.YES_NO_OPTION, "Invite" + inviter, Resources //$NON-NLS-1$
                .getString("Chat.ignore_invitation")); //$NON-NLS-1$
          if (i == 0) {
            doInvite(inviter, roomName);
          }
          else {
            MultiUserChat.decline(conn, room, inviter, "");                 //$NON-NLS-1$
          }
        }
      }};

    // Listen for other clients leaving a room I own and revoke their membership
    userListener = new DefaultParticipantStatusListener() {
      @Override
      public void kicked(String participant, String arg1, String arg2) {
        revokeMembership(participant);
      }
      @Override
      public void left(String participant) {
        revokeMembership(participant);
      }
      private void revokeMembership(String participant) {
        final LockableRoom room = getCurrentRoom();
        if (room.isLocked() && room.isOwner(me.getJid())) {
          try {
            final String jid = JabberPlayer.xmppAddressToJid(participant);
            currentChat.revokeMembership(playerMgr.getPlayer(jid).getId());
          }
          catch (XMPPException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
          }
        }
      }
    };
  }

  @Override
  public void addPropertyChangeListener(String propertyName, PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(propertyName, l);
  }

  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  @Override
  public boolean isConnected() {
    return conn != null && conn.isConnected();
  }

  @Override
  public void sendToOthers(Command c) {
    if (currentChat != null) {
      try {
        currentChat.sendMessage(encodeMessage(encoder.encode(c)));
      }
      // FIXME: review error message
      catch (XMPPException e) {
        reportXMPPException(e);
      }
    }
  }

  @Override
  public void setConnected(boolean connect) {
    if (connect) {
      if (!isConnected()) {
        if (conn != null) {
          conn.disconnect();
        }
        try {
          playerMgr.clear();
          roomMgr.clear();
          String username = account.getUserName();
          String password = account.getPassword();
          me = playerMgr.getPlayerByLogin(this, account.getUserName());

          final GameModule g = GameModule.getGameModule();
          final SimpleStatus s = (SimpleStatus) me.getStatus();
          s.updateStatus();
          me.setStatus(s);
          me.setName((String) g.getPrefs().getValue(GameModule.REAL_NAME));

          ConnectionConfiguration config = new ConnectionConfiguration(host, port);
          config.setCompressionEnabled(true);
          config.setDebuggerEnabled(XMPPConnection.DEBUG_ENABLED);
          config.setReconnectionAllowed(true);

          conn = new XMPPConnection(config);
          conn.connect();
          conn.addConnectionListener(new ConnectionListener());
          try {
            conn.login(username, password, "VASSAL"); //$NON-NLS-1$
          }
          // FIXME: review error message
          catch (XMPPException e) {
            // Create the account if it doesn't exist
            Map<String, String> attributes = new HashMap<>();
            attributes.put("name", me.getName()); //$NON-NLS-1$
            try {
              conn.getAccountManager().createAccount(username, password, attributes);
            }
            // FIXME: review error message
            catch (XMPPException createAccountError) {
              if (createAccountError.getXMPPError() != null && createAccountError.getXMPPError().getCode() == 409) {
                // Account already exists. Password is incorrect
                fireStatus(Resources.getString("Chat.invalid_password", username)); //$NON-NLS-1$
                setConnected(false);
                return;
              }
              else {
                setConnected(false);
                throw createAccountError;
              }
            }

            // ejabberd servers require a reconnection after an account creation before
            // they will allow the user to login
            conn.disconnect();
            conn.connect();

            // Retry the login
            try {
              conn.login(username, password, "VASSAL"); //$NON-NLS-1$
            }
            catch (XMPPException retryError) {
              setConnected(false);
              throw retryError;
            }
            VCard c = new VCard();
            c.setNickName(me.getName());
            c.save(conn);
          }
          monitor = new MonitorRooms();
          monitor.init();
          propSupport.firePropertyChange(CONNECTED, null, Boolean.TRUE);
          fireStatus(Resources.getString("Server.connected", host + ":" + port)); //$NON-NLS-1$ //$NON-NLS-2$
          setRoom(defaultRoom);
          GameModule.getGameModule().addIdChangeListener(idChangeListener);
          MultiUserChat.addInvitationListener(conn, inviteListener);
        }
        // FIXME: review error message
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
        tidyConnection();
      }
    }
  }

  private void tidyConnection() {
    conn = null;
    monitor = null;
    currentChat = null;
    propSupport.firePropertyChange(CONNECTED, null, Boolean.FALSE);
    playerMgr.clear();
    roomMgr.clear();
    fireStatus(Resources.getString("Server.disconnected", host + ":" + port)); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private void leaveCurrentRoom() {
    if (currentChat != null) {
      currentChat.leave();
      currentChat.removeMessageListener(this);
      currentChat.removeUserStatusListener(kickListener);
      currentChat.removeParticipantStatusListener(userListener);
      currentChat = null;
    }
  }

  @Override
  public void initializeControls(ChatServerControls controls) {
    playerStatusControls.initializeControls(controls);
    // messageBoardControls.initializeControls(controls);
    roomControls.initializeControls(controls);
    // serverStatusControls.initializeControls(controls);
    controls.setRoomControlsVisible(true);
    GameModule.getGameModule().addCommandEncoder(synchEncoder);
    GameModule.getGameModule().addCommandEncoder(privateChatEncoder);
    GameModule.getGameModule().addCommandEncoder(soundEncoder);
    controls.getRoomTree().setCellRenderer(new LockableRoomTreeRenderer());
  }

  @Override
  public void uninitializeControls(ChatServerControls controls) {
    // messageBoardControls.uninitializeControls(controls);
    roomControls.uninitializeControls(controls);
    playerStatusControls.uninitializeControls(controls);
    // serverStatusControls.uninitializeControls(controls);
    GameModule.getGameModule().removeCommandEncoder(synchEncoder);
    GameModule.getGameModule().removeCommandEncoder(privateChatEncoder);
    GameModule.getGameModule().removeCommandEncoder(soundEncoder);
  }

  @Override
  public void processPacket(Packet packet) {
    Message m = (Message) packet;
    if (!m.getFrom().equals(currentChat.getRoom() + "/" + currentChat.getNickname())) { //$NON-NLS-1$
      propSupport.firePropertyChange(INCOMING_MSG, null, decodeMessage(m.getBody()));
    }
  }

  public void processServerMessage(String subject, String message) {
    final GameModule g = GameModule.getGameModule();
    g.warn("##### " + Resources.getString("JabberClient.message_from_admin", host+":"+port)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    if (subject != null) {
      g.warn(Resources.getString("JabberClient.subject")+subject); //$NON-NLS-1$
    }
    g.warn(message);
    g.warn("##### " + Resources.getString("JabberClient.end_message")); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public Room getRoom() {
    return monitor.getCurrentRoom();
  }

  public JabberRoom getRoomByName(String name) {
    return roomMgr.getRoomByName(this, name);
  }

  public String getCurrentRoomJID() {
    return currentChat == null ? null : currentChat.getRoom();
  }

  public LockableRoom getCurrentRoom() {
    return monitor.getCurrentRoom();
  }

  public void setRoom(String roomName) {
    setRoom(roomMgr.getRoomByName(this, roomName));
  }

  @Override
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
        final String failedToJoinMessage = newRoom.canJoin(me);
        if (failedToJoinMessage != null) {
          fireStatus(Resources.getString("Chat.failed_to_join", newRoom.getName(), failedToJoinMessage)); //$NON-NLS-1$
          return;
        }
        leaveCurrentRoom();
        currentChat = newRoom.join(this, (JabberPlayer) getUserInfo());
        if (newRoom.isOwnedByMe()) {
          currentChat.addParticipantStatusListener(userListener);
        }
        fireStatus(Resources.getString("Chat.joined_room", newRoom.getName())); //$NON-NLS-1$
        if (!newRoom.isOwnedByMe() && ! isDefaultRoom(newRoom)) {
          new SynchAction(newRoom.getOwningPlayer(), this).actionPerformed(null);
          GameModule.getGameModule().warn(Resources.getString("Chat.synchronize_complete")); //$NON-NLS-1$
        }
        else {
          SynchAction.clearSynchRoom();
        }
        currentChat.addUserStatusListener(kickListener);
        monitor.sendRoomChanged();
        monitor.sendStatus(me, newRoom);
      }
    }
    // FIXME: review error message
    catch (XMPPException e) {
      reportXMPPException(e);
      String mess = null;
      if (e.getXMPPError() != null) {
        final XMPPError error = e.getXMPPError();
        if (error.getCode() == 407) {
          mess = Resources.getString("Chat.not_a_member"); //$NON-NLS-1$
        }
        else {
          mess = e.getXMPPError().getMessage();
          if (mess == null) {
            mess = e.getXMPPError().getCondition();
          }
        }
      }
      else {
        mess = e.getMessage();
      }
      fireStatus(Resources.getString("Chat.failed_to_join", newRoom.getName(), mess)); //$NON-NLS-1$
    }
  }

  @Override
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

  @Override
  public Player getUserInfo() {
    return playerMgr.getPlayerByLogin(this, account.getUserName());
  }

  @Override
  public void setUserInfo(Player p) {
    if (monitor != null) {
      monitor.sendStatus((JabberPlayer) p);
    }
  }

  @Override
  public String getDefaultRoomName() {
    return defaultRoom.getName();
  }

  @Override
  public boolean isDefaultRoom(Room r) {
    return r == null ? false : r.getName().equals(getDefaultRoomName());
  }

  @Override
  public void sendTo(Player recipient, Command c) {
    Chat chat = conn.getChatManager().createChat(((JabberPlayer) recipient).getJid(), null);
    try {
      chat.sendMessage(encodeMessage(encoder.encode(c)));
    }
    // FIXME: review error message
    catch (XMPPException e) {
      reportXMPPException(e);
    }
  }


  /** Can a player be invited to this room? */
  @Override
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
  @Override
  public void sendInvite(Player invitee) {
    try {
      currentChat.grantMembership(invitee.getId());
    }
    catch (XMPPException e) {
      ErrorDialog.bug(new Throwable("Unable to grant membership to room "+getCurrentRoom().getName()+" to player "+invitee.getId(), e)); //$NON-NLS-1$ //$NON-NLS-2$
    }
    currentChat.invite(((JabberPlayer) invitee).getRawJid(), INVITE); //$NON-NLS-1$
  }

  /** Process an invitation */
  @Override
  public void doInvite(String playerId, String roomName) {
    setRoom(roomName);
  }

  /** Is a player kickable from this room? */
  @Override
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
  @Override
  public void doKick(Player kickee) {
    try {
      currentChat.kickParticipant(((JabberPlayer) kickee).getLoginName(), ""); //$NON-NLS-1$
    }
    catch (XMPPException e) {
      // TODO Error - unable to kick, I must not be owner???
      e.printStackTrace();
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
    return "vassal-" + GameModule.getGameModule().getGameName(); //$NON-NLS-1$
  }

  public String getConferenceService() {
    return conferenceService;
  }

  public static String unescapeNode(String node) {
    return StringUtils.unescapeNode(node);
  }

  /**
   * Messages must be encoded to pass through Jabber:
   * 1. To remove Escape characters (Vassal sub-command separator)
   * 2. To hide the raw Vassal commands from observers on the chat room using a Jabber Client.
   *
   * @param clearText
   * @return encoded text
   */
  protected String encodeMessage(String clearText) {
    try (FastByteArrayOutputStream ba = new FastByteArrayOutputStream();
         ObfuscatingOutputStream out = new ObfuscatingOutputStream(ba)) {
      out.write(clearText.getBytes(StandardCharsets.UTF_8));
      return new String(ba.toByteArray());
    }
    catch (IOException e) {
      e.printStackTrace();
      return "";
    }
  }

  /**
   * Encode text encoded by encodeMessage
   *
   * @param encodedMessage
   * @return decoded text
   */
  protected String decodeMessage(String encodedMessage) {
    try (ByteArrayInputStream ba = new ByteArrayInputStream(encodedMessage.getBytes());
         DeobfuscatingInputStream in = new DeobfuscatingInputStream(ba)) {
      return IOUtils.toString(in, StandardCharsets.UTF_8);
    }
    catch (IOException e) {
      e.printStackTrace();
      return "";
    }
  }

  /**
   * Toggle the lock state on the room.
   */
  @Override
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
   * VASSAL clients join a common room, named for the module, from which they communicate information about which
   * players have joined which rooms, etc.
   *
   * @author rodneykinney
   *
   */
  private class MonitorRooms implements PacketListener, ParticipantStatusListener {
    private static final String ROOM_CHANGE_ACTION = "changedRoom"; //$NON-NLS-1$
    private MultiUserChat monitorRoom;
    private Comparator<Room> roomSortOrder = new Comparator<>() {
      @Override
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
      new ListenForChat().addTo(conn);
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
        if (ex.getXMPPError().getCode() != 403) {
          throw ex;
        }
      }
      sendStatus(me);
    }

    public String getMonitorRoomJID() {
      return StringUtils.escapeNode(getModule()) + "@" + getConferenceService(); //$NON-NLS-1$
    }

    protected void sendStatus(JabberPlayer p) {
      sendStatus(p, null, p.getJoinedRoom());
    }

    protected void sendStatus(JabberPlayer player, JabberRoom room) {
      sendStatus(player, null, room);
    }

    protected void sendStatus(JabberPlayer player, String recipient, JabberRoom room) {
      final SimpleStatus s = (SimpleStatus) player.getStatus();
      final Presence p = new Presence(Presence.Type.available);
      p.setStatus(""); //$NON-NLS-1$
      p.setMode(Presence.Mode.chat);
      p.setProperty(SimpleStatus.LOOKING, s.isLooking());
      p.setProperty(SimpleStatus.AWAY, s.isAway());
      p.setProperty(SimpleStatus.IP, s.getIp());
      p.setProperty(SimpleStatus.CLIENT, s.getClient());
      p.setProperty(SimpleStatus.MODULE_VERSION, s.getModuleVersion());
      p.setProperty(SimpleStatus.CRC, s.getCrc());
      p.setProperty(REAL_NAME, player.getName()); //$NON-NLS-1$
      if (room != null) {
        p.setProperty(ROOM_CONFIG, room.encodeConfig());
        p.setProperty(ROOM_JID, room.getJID());
        p.setProperty(ROOM_NAME, room.getName());
      }
      p.setTo(recipient == null ? monitorRoom.getRoom() : recipient);
      conn.sendPacket(p);
    }

    public Room[] getAvailableRooms() {
      Map<JabberRoom, List<JabberPlayer>> occupants = new HashMap<>();
      for (JabberPlayer p : playerMgr.getAllPlayers()) {
        JabberRoom room = p.getJoinedRoom();
        if (room != null) {
          List<JabberPlayer> l = occupants.computeIfAbsent(room, k -> new ArrayList<>());
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
        room.setPlayers(l.toArray(new JabberPlayer[0]));
      }
      Room[] roomArray = rooms.toArray(new Room[0]);
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
     * Take the room-local JID for a player (room@conference.server/nick) and change it into an absolute address for
     * that player (login@server/VASSAL)
     *
     * @param jid
     * @return
     */
    public String getAbsolutePlayerJID(String jid) {
      return StringUtils.parseResource(jid) + "@" + host + JID_RESOURCE; //$NON-NLS-1$
    }

    private void sendRoomQuery(String jid) {
      DiscoverItems disco = new DiscoverItems();
      disco.setType(IQ.Type.GET);
      disco.setTo(jid);
      disco.setNode(QUERY_ROOMS);
      conn.sendPacket(disco);
    }

    @Override
    public void processPacket(Packet packet) {
      Message m = (Message) packet;
      if (ROOM_CHANGE_ACTION.equals(m.getBody())) {
        String jid = getAbsolutePlayerJID(packet.getFrom());
        playerMgr.getPlayer(getAbsolutePlayerJID(packet.getFrom()));
        sendRoomQuery(jid);
      }
    }

    @Override
    public void joined(String participant) {
      playerMgr.getPlayer(getAbsolutePlayerJID(participant));
    }

    @Override
    public void left(String participant) {
      String jid = getAbsolutePlayerJID(participant);
      playerMgr.deletePlayer(jid);
      fireRoomsUpdated();
    }

    @Override
    public void kicked(String participant, String actor, String reason) {
    }

    @Override
    public void voiceGranted(String participant) {
    }

    @Override
    public void voiceRevoked(String participant) {
    }

    @Override
    public void banned(String participant, String actor, String reason) {
    }

    @Override
    public void membershipGranted(String participant) {
    }

    @Override
    public void membershipRevoked(String participant) {
    }

    @Override
    public void moderatorGranted(String participant) {
    }

    @Override
    public void moderatorRevoked(String participant) {
    }

    @Override
    public void ownershipGranted(String participant) {
    }

    @Override
    public void ownershipRevoked(String participant) {
    }

    @Override
    public void adminGranted(String participant) {
    }

    @Override
    public void adminRevoked(String participant) {
    }

    @Override
    public void nicknameChanged(String participant, String newNickname) {
    }
    private class TrackStatus extends PacketProcessor {
      String prefix;

      private PacketFilter changeStatusFilter = new PacketFilter() {
        @Override
        public boolean accept(Packet packet) {
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
        }};

      public TrackStatus(String prefix) {
        this.prefix = prefix;
      }

      @Override
      public boolean acceptPacket(Packet packet) {
        return packet instanceof Presence;
      }

      @Override
      public void process(Packet packet) {
        // Process a change of status by another user
        if (changeStatusFilter.accept(packet)) {

          final Presence p = (Presence) packet;
          final JabberPlayer player = playerMgr.getPlayer(getAbsolutePlayerJID(p
              .getFrom()));
          SimpleStatus status = (SimpleStatus) player.getStatus();
          final String profile = status == null ? "" : status.getProfile(); //$NON-NLS-1$
          final Object looking = p.getProperty(SimpleStatus.LOOKING);
          final Object away = p.getProperty(SimpleStatus.AWAY);
          final String ip = p.getProperty(SimpleStatus.IP).toString();
          final String client = p.getProperty(SimpleStatus.CLIENT).toString();
          final String moduleVersion = p.getProperty(SimpleStatus.MODULE_VERSION)
              .toString();
          final String crc = p.getProperty(SimpleStatus.CRC).toString();
          status = new SimpleStatus(looking == null ? false : (Boolean) looking,
              away == null ? false : (Boolean) away, profile, client, ip,
              moduleVersion, crc);
          player.setStatus(status);
          player.setName(String.valueOf(p.getProperty(REAL_NAME)));
          fireRoomsUpdated();
        }
        // Track room ownership
        if (packet instanceof Presence) {
          final Presence p = (Presence) packet;
          if (p.getType().equals(Presence.Type.available)) {
            PacketExtension ext = p.getExtension(QUERY_USER);
            JabberRoom room = null;
            if (ext instanceof MUCUser){
              final String affiliation = ((MUCUser) ext).getItem().getAffiliation();
              final String jid = playerMgr.getPlayer(getAbsolutePlayerJID(p.getFrom())).getJid();
              String roomJid = (String) p.getProperty(ROOM_JID);
              final String roomConfig = (String) p.getProperty(ROOM_CONFIG);
              final String roomName = (String) p.getProperty(ROOM_NAME);
              if (roomJid == null) {
                roomJid = StringUtils.parseName(p.getFrom()) + "@" + getConferenceService(); //$NON-NLS-1$
              }
              room = roomMgr.getRoomByJID(JabberClient.this, roomJid, roomName);

              if (room != null) {
                if (OWNER.equals(affiliation)) {
                  room.addOwner(jid);
                }
                else {
                  room.removeOwner(jid);
                }
              }

              if (roomConfig != null && room != null) {
                room.decodeConfig(roomConfig);
              }
            }
          }
        }
      }
    }

    /**
     * Track available rooms
     *
     */
    private class TrackRooms extends PacketProcessor {
      private PacketFilter roomResponseFilter = new AndFilter(new IQTypeFilter(IQ.Type.RESULT), new PacketTypeFilter(DiscoverItems.class));
      private PacketFilter newPlayerFilter = new AndFilter(new PacketTypeFilter(Presence.class), new FromContainsFilter(getMonitorRoomJID()));

      public TrackRooms() {
      }

      @Override
      public void process(Packet packet) {
        if (roomResponseFilter.accept(packet)) {
          final DiscoverItems result = (DiscoverItems) packet;
          final JabberPlayer player = playerMgr.getPlayer(packet.getFrom());
          // Collect the entityID for each returned item
          for (Iterator<DiscoverItems.Item> items = result.getItems(); items.hasNext();) {
            final String roomJID = items.next().getEntityID();
            final JabberRoom room = roomMgr.getRoomByJID(JabberClient.this, roomJID);
            try {
              room.setInfo(MultiUserChat.getRoomInfo(JabberClient.this.getConnection(), roomJID));
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

      @Override
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

    /**
     * Listen for any Private chat and pass it on to
     * the client. (Also Synch's, private messages and wake ups).
     */
    private class ListenForChat extends PacketProcessor {

      private PacketFilter chatFilter = new MessageTypeFilter(Message.Type.chat);
      private PacketFilter serverMessageFilter = new MessageTypeFilter(Message.Type.normal);

      @Override
      protected boolean acceptPacket(Packet packet) {
        if (chatFilter.accept(packet)) {
          return true;
        }
        if (serverMessageFilter.accept(packet)) {
          return packet.getFrom().equals(JabberClient.this.getConnection().getHost());
        }
        return false;
      }

      @Override
      protected void process(Packet packet) {
        if (chatFilter.accept(packet)) {
          JabberClient.this.processPacket(packet);
        }
        else {
          final Message m = ((Message) packet);
          JabberClient.this.processServerMessage(m.getSubject(), m.getBody());
        }
      }
    }
  }

  public CommandEncoder getEncoder() {
    return encoder;
  }

  public void setEncoder(CommandEncoder encoder) {
    this.encoder = encoder;
  }

  @Override
  public ModuleSummary[] getHistory(String timeRange) {
    return new ModuleSummary[0];
  }

  @Override
  public ModuleSummary[] getStatus() {
    ArrayList<ModuleSummary> entries = new ArrayList<>();
    try {
      for (HostedRoom room : MultiUserChat.getHostedRooms(conn, conferenceService)) {
        MultiUserChat.getRoomInfo(conn, room.getJid());
      }
    }
    // FIXME: review error message
    catch (XMPPException e) {
      e.printStackTrace();
    }
    return entries.toArray(new ModuleSummary[0]);
  }

  @Override
  public String[] getSupportedTimeRanges() {
    return new String[0];
  }
  private class ConnectionListener implements org.jivesoftware.smack.ConnectionListener {
    @Override
    public void connectionClosed() {
    }

    @Override
    public void connectionClosedOnError(Exception e) {
      String msg = e.getMessage();
      if (e instanceof XMPPException) {
        XMPPException xe = (XMPPException) e;
        if (xe.getStreamError() != null && "conflict".equals(xe.getStreamError().getCode())) { //$NON-NLS-1$
          msg = Resources.getString("Server.account_in_use"); //$NON-NLS-1$
        }
      }
      if (msg != null) {
        fireStatus(msg);
      }
      setConnected(false);
    }

    @Override
    public void reconnectingIn(int seconds) {
    }

    @Override
    public void reconnectionFailed(Exception e) {
    }

    @Override
    public void reconnectionSuccessful() {
    }
  }

  public static void main(String[] args) {
    XMPPConnection.DEBUG_ENABLED = true;
    CommandEncoder c = new CommandEncoder() {
      @Override
      public Command decode(String command) {
        System.err.println(command);
        return null;
      }

      @Override
      public String encode(Command c) {
        return null;
      }
    };
    final String username = args.length == 0 ? "test" : args[0]; //$NON-NLS-1$
    final String password = args.length == 0 ? "test" : args[1]; //$NON-NLS-1$
    // JabberClient client = new JabberClient(c, "63.144.41.3", 5222, username, password);
    AccountInfo account = new AccountInfo() {
      @Override
      public String getPassword() {
        return password;
      }

      @Override
      public String getUserName() {
        return username;
      }

      @Override
      public String getModule() {
        return "JabberTestModule";
      }

      @Override
      public String getRealName() {
        return username;
      }
    };
    JabberClient client = new JabberClient(c, "localhost", 5222, account); //$NON-NLS-1$
    client.addPropertyChangeListener(new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        System.err.println(evt.getPropertyName() + "=" + evt.getNewValue()); //$NON-NLS-1$
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

  @Override
  public String playerToString(Player p) {
    return ((JabberPlayer)p).getJid();
  }

  @Override
  public Player stringToPlayer(String s) {
    return playerMgr.getPlayer(s);
  }

  public static String testConnection(String host, String port, String login, String passwd) {
    final StringBuilder text = new StringBuilder(Resources.getString("JabberClient.testing_connection")+host+":"+port+" "+login+"/"+passwd).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

    if (host.length() == 0) {
      return text.append(Resources.getString("JabberClient.error_no_host")).toString(); //$NON-NLS-1$
    }

    int portNo = 0;
    try {
      portNo = Integer.parseInt(port);
    }
    catch (NumberFormatException e) {
      return text.append(Resources.getString("JabberClient.error_port_number")).toString();       //$NON-NLS-1$
    }

    final ConnectionConfiguration config = new ConnectionConfiguration(host, portNo);
    config.setCompressionEnabled(true);
    config.setDebuggerEnabled(XMPPConnection.DEBUG_ENABLED);
    config.setReconnectionAllowed(true);
    final XMPPConnection conn = new XMPPConnection(config);

    try {
      text.append(Resources.getString("JabberClient.attempting_to_connect")).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
      conn.connect();
      text.append(Resources.getString("JabberClient.success")).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
      text.append(Resources.getString("JabberClient.attempting_to_login")).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
      try {
        conn.login(login, passwd, "VASSAL"); //$NON-NLS-1$
        text.append(Resources.getString("JabberClient.success")); //$NON-NLS-1$
      }
      catch (XMPPException e) {
        text.append(Resources.getString("JabberClient.login_failed")).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
        text.append(Resources.getString("JabberClient.attempting_to_create")).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
        final Map<String, String> attributes = new HashMap<>();
        attributes.put("name", GameModule.getUserId()); //$NON-NLS-1$
        try {
          conn.getAccountManager().createAccount(login, passwd,
              attributes);
          text.append(Resources.getString("JabberClient.success")); //$NON-NLS-1$
        }
        catch (XMPPException ex) {
          text.append(Resources.getString("JabberClient.failed")).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
          if (ex.getXMPPError() != null && ex.getXMPPError().getCode() == 409) {
            // Account already exists. Password is incorrect
            text.append(Resources.getString("Chat.invalid_password")).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
          }
          else {
            text.append(formatXMPPError(ex));
          }
        }
      }
    }
    catch (XMPPException e) {
      text.append(Resources.getString("JabberClient.failed")).append("\n"); //$NON-NLS-1$ //$NON-NLS-2$
      text.append(formatXMPPError(e));
    }
    finally {
      conn.disconnect();
    }
    return text.toString();
  }

  private static String formatXMPPError(XMPPException e) {
    final XMPPError error = e.getXMPPError();
    if (error == null) {
      return Resources.getString("Server.server_error", e.getMessage(), "", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
    else {
      return Resources.getString("Server.server_error", e //$NON-NLS-1$
          .getXMPPError().getMessage(), e.getXMPPError().getCondition(), e
          .getXMPPError().getCode());
    }
  }
}
