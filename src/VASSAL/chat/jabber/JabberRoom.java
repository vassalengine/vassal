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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.Occupant;
import org.jivesoftware.smackx.muc.RoomInfo;

import VASSAL.chat.LockableRoom;
import VASSAL.chat.SimpleRoom;
import VASSAL.tools.SequenceEncoder;

public class JabberRoom extends SimpleRoom implements LockableRoom {
  private static final String CONFIG_MEMBERSONLY = "muc#roomconfig_membersonly";
  private String jid;
  private RoomInfo info;
  private boolean ownedByMe;
  private JabberClient client;
  private ArrayList<String> owners = new ArrayList<String>();

  private JabberRoom(String name, String jid, RoomInfo info, JabberClient client) {
    super(name);
    this.jid = jid;
    this.info = info;
    this.client = client;
  }

  public static String jidToName(String jid) {
    final String roomRef = jid.split("@")[0];
    final String[] parts = roomRef.split("2f");
    return parts[parts.length-1];  
  }
  
  public String getJID() {
    return jid;
  }

  public boolean isLocked() {
    return info != null && info.isMembersOnly();
  }

  public void setInfo(RoomInfo info) {
    this.info = info;
  }
  
  public void toggleLock(MultiUserChat muc) {
    try {
      if (!isLocked()) {
        lock(muc);
      }
      else {
        unlock(muc);
      }
      info = MultiUserChat.getRoomInfo(client.getConnection(), jid);
    }
    catch (XMPPException e) {
      e.printStackTrace();
      return;
    }
  }

  protected void lock(MultiUserChat muc) throws XMPPException {
    final Form form = muc.getConfigurationForm().createAnswerForm();
    form.setAnswer(CONFIG_MEMBERSONLY, true);
    muc.sendConfigurationForm(form);
  }

  protected void unlock(MultiUserChat muc) throws XMPPException {
    final Form form = muc.getConfigurationForm().createAnswerForm();
    form.setAnswer(CONFIG_MEMBERSONLY, false);
    muc.sendConfigurationForm(form);
  }

  public MultiUserChat join(JabberClient client, JabberPlayer me)
      throws XMPPException {
    MultiUserChat chat = new MultiUserChat(client.getConnection(), getJID());
    chat.join(StringUtils.parseName(me.getJid()));

    if (!chat.isJoined()) {
      return null;
    }
    
    try {
      // This is necessary to create the room if it doesn't already exist
      chat.sendConfigurationForm(new Form(Form.TYPE_SUBMIT));
      ownedByMe = true;
    }
    catch (XMPPException e) {
      // 403 code means the room already exists and user is not an owner
      // Anything else is a problem.
      if (e.getXMPPError() != null && e.getXMPPError().getCode() != 403) {
        throw e;
      }
    }

    findRoomOwners(chat);
    
    try {
      chat.changeSubject(getName());
    }
    catch (XMPPException e) {
      // No error - Room already exists but we're not the owner
    }

    chat.addMessageListener(client);
    return chat;
  }

  public void updateRoomOwners() {
    findRoomOwners(new MultiUserChat(client.getConnection(), getJID()));
  }
  
  protected void findRoomOwners(MultiUserChat chat) {
    owners = new ArrayList<String>();
    
    for (Iterator<String> i = chat.getOccupants(); i.hasNext();) {
      final Occupant occupant = chat.getOccupant(i.next());
      if ("owner".equals(occupant.getAffiliation())) {
        owners.add(occupant.getJid());
      }
    }   
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
  
  public boolean isOwner(String jid) {
    return owners.contains(jid);
  }

  public String getOwners() {
    return encodeOwners();
  }
  
  public void setOwner(String ownerList) {
    decodeOwners(ownerList);
  }
  
  protected String encodeOwners() {
    if (owners.isEmpty()) {
      return "";
    }
    SequenceEncoder se = new SequenceEncoder(',');
    for (String owner : owners) {
      se.append(owner);
    }
    return se.getValue();
  }
  
  protected void decodeOwners(String s) {
    owners.clear();
    for (SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ','); sd.hasNext();) {
      owners.add(sd.nextToken());
    }
  }
  
  public static class Manager {
    private Map<String, JabberRoom> jidToRoom = new HashMap<String, JabberRoom>();

    public synchronized JabberRoom getRoomByJID(JabberClient client, String jid) {
      if (jid == null) {
        return null;
      }
      JabberRoom newRoom = jidToRoom.get(jid);
      if (newRoom == null) {
        String subject = "<no name>";
        RoomInfo info = null;
        try {
          info = MultiUserChat.getRoomInfo(client.getConnection(), jid);
          subject = info.getSubject();
        }
        // FIXME: review error message
        catch (XMPPException e) {
          e.printStackTrace();
          return null;
        }
        newRoom = new JabberRoom(subject, jid, info, client);
        jidToRoom.put(jid, newRoom);
      }
      return newRoom;
    }

    public synchronized JabberRoom getRoomByName(JabberClient client,
        String name) {
      String jid = StringUtils.escapeNode(client.getModule() + "/" + name)
          .toLowerCase()
          + "@" + client.getConferenceService();
      JabberRoom room = jidToRoom.get(jid);
      if (room == null) {
        room = new JabberRoom(name, jid, null, client);
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
}
