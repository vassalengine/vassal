/*
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

import java.util.HashMap;
import java.util.Map;

import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.RoomInfo;

import VASSAL.chat.LockableRoom;
import VASSAL.chat.Player;
import VASSAL.chat.SimpleRoom;

public class JabberRoom extends SimpleRoom implements LockableRoom {
  private static final String JABBER_MEMBERSONLY = "muc#roomconfig_membersonly";  //$NON-NLS-1$
  private static final String JABBER_ALLOW_INVITES = "muc#roomconfig_allowinvites"; //$NON-NLS-1$
  private static final String JABBER_CHANGE_SUBJECT = "muc#roomconfig_changesubject"; //$NON-NLS-1$
  private static final String JABBER_MODERATED = "muc#roomconfig_moderatedroom"; //$NON-NLS-1$
  private static final String JABBER_PASSWORD_PROTECTED = "muc#roomconfig_passwordprotectedroom"; //$NON-NLS-1$
  private static final String JABBER_PERSISTENT = "muc#roomconfig_persistentroom"; //$NON-NLS-1$
  private static final String JABBER_PUBLIC_ROOM = "muc#roomconfig_publicroom"; //$NON-NLS-1$
  private static final String JABBER_ROOM_DESC = "muc#roomconfig_roomdesc"; //$NON-NLS-1$
  
  private String jid;
  private RoomInfo info;
  private boolean ownedByMe;

  private JabberRoom(String name, String jid, RoomInfo info) {
    super(name);
    this.jid = jid;
    this.info = info;
  }

  public String getJID() {
    return jid;
  }

  public boolean isLocked() {
    return info != null && info.isMembersOnly();
  }

  public MultiUserChat join(JabberClient client, JabberPlayer me) throws XMPPException {
    MultiUserChat chat = new MultiUserChat(client.getConnection(), getJID());
    chat.join(StringUtils.parseName(me.getJid()));
    
    if (!chat.isJoined()) {
      return null;
    }
    
    try {
      // This is necessary to create the room if it doesn't already exist
      // Configure the options we needs explicitly, don't depend on the server supplied defaults
      final Form configForm = chat.getConfigurationForm().createAnswerForm();
      configForm.setAnswer(JABBER_MEMBERSONLY, false); 
      configForm.setAnswer(JABBER_ALLOW_INVITES, false); 
      configForm.setAnswer(JABBER_CHANGE_SUBJECT, false);
      configForm.setAnswer(JABBER_MODERATED, false);
      configForm.setAnswer(JABBER_PASSWORD_PROTECTED, false);
      configForm.setAnswer(JABBER_PERSISTENT, false);
      configForm.setAnswer(JABBER_PUBLIC_ROOM, true);
      configForm.setAnswer(JABBER_ROOM_DESC, getName());
      
      chat.sendConfigurationForm(configForm);
      ownedByMe = true;
    }
    catch (XMPPException e) {
      // 403 code means the room already exists and user is not an owner
      if (e.getXMPPError() != null && e.getXMPPError().getCode() != 403) {
        throw e;
      }
    }

    try {
      chat.changeSubject(getName());
    }
    catch (XMPPException e) {
      // Room already exists but we're not the owner
    }

    chat.addMessageListener(client);
    return chat;
  }

  public boolean equals(Object o) {
    if (o instanceof JabberRoom) {
      JabberRoom r = (JabberRoom) o;
      return r.jid.equals(jid);
    }
    else {
      return false;
    }
  }

  public int hashCode() {
    return jid.hashCode();
  }

  public boolean isOwnedByMe() {
    return ownedByMe;
  }
  public static class Manager {
    private Map<String, JabberRoom> jidToRoom = new HashMap<String, JabberRoom>();

    public synchronized JabberRoom getRoomByJID(JabberClient client, String jid) {
      if (jid == null) {
        return null;
      }
      JabberRoom newRoom = jidToRoom.get(jid);
      if (newRoom == null) {
        String roomName = "???"; //$NON-NLS-1$
        RoomInfo info = null;
        try {
          info = MultiUserChat.getRoomInfo(client.getConnection(), jid);
          roomName = info.getDescription();
        }
        // FIXME: review error message
        catch (XMPPException e) {
          e.printStackTrace();
        }
        newRoom = new JabberRoom(roomName, jid, info);
        jidToRoom.put(jid, newRoom);
      }
      return newRoom;
    }

    public synchronized JabberRoom getRoomByName(JabberClient client, String name) {
      String jid = StringUtils.escapeNode(client.getModule() + "-" + name).toLowerCase() + "@" + client.getConferenceService(); //$NON-NLS-1$ //$NON-NLS-2$
      JabberRoom room = jidToRoom.get(jid);
      if (room == null) {
        room = new JabberRoom(name, jid, null);
        jidToRoom.put(jid, room);
      }
      return room;
    }

    public void deleteRoom(String jid) {
      jidToRoom.remove(jid);
    }
    
    public synchronized void clear() {
      jidToRoom.clear();
    }
  }
  public Player getOwningPlayer() {
    // TODO Auto-generated method stub
    return null;
  }

  public boolean isOwner(String jid) {
    // TODO Auto-generated method stub
    return false;
  }
}
