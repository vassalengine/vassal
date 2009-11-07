/*
 * $Id: JabberServerStatus.java 6210 2009-11-01 11:16:06Z swampwallaby $
 *
 * Copyright (c) 2009 by Brent Easton
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
 */package VASSAL.chat.jabber;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.muc.HostedRoom;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.RoomInfo;

import VASSAL.chat.Room;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleRoom;
import VASSAL.tools.PropertiesEncoder;

public class JabberServerStatus implements ServerStatus {

  protected JabberClient client;
  protected final HashMap<String,ModuleSummary> entries = new HashMap<String,ModuleSummary>();
  
  public JabberServerStatus(JabberClient client) {
    this.client = client;
  }
  
  public ModuleSummary[] getHistory(String timeRange) {
    return new ModuleSummary[0];  // No History
  }

  public String[] getSupportedTimeRanges() {
    return new String[0]; // No History
  }
  
  public ModuleSummary[] getStatus() {

    Collection<HostedRoom> rooms;
    
    try {
      rooms = MultiUserChat.getHostedRooms(client.getConnection(), "conference.ziggy");
    }
    catch (XMPPException e) {
      return new ModuleSummary[0];
    }
    
    for (HostedRoom room : rooms) {
      final String name = StringUtils.unescapeNode(room.getName());
      final String[] parts = name.split("/");
      if (parts.length == 2) {  // Module control room
        final String moduleKey = parts[1];
        ModuleSummary ms = entries.get(moduleKey);
        String moduleName = moduleKey;
        if (ms == null) {
          
          try {
            RoomInfo info = MultiUserChat.getRoomInfo(client.getConnection(), room.getJid());
            moduleName = info.getSubject();
          }
          catch (XMPPException e) {
            // No error
          }       
          ms = new ModuleSummary(moduleName); 
        } 
        else {
          ms.setModuleName(moduleName);
        }
        entries.put(moduleKey, ms);
      }
      
      if (parts.length == 3) { // User Room
        final String moduleKey = parts[1];
        final String roomKey = parts[2];
        String roomName = roomKey;
        ModuleSummary ms = entries.get(moduleKey);
        if (ms == null) {
          ms = new ModuleSummary(moduleKey);          
        }
        int occupantCount = 0;
        try {
          RoomInfo info = MultiUserChat.getRoomInfo(client.getConnection(), room.getJid());
          occupantCount = info.getOccupantsCount();
          try {
            roomName = new PropertiesEncoder(info.getSubject()).getProperties().getProperty(JabberRoom.CONFIG_NAME);
          }
          catch (IOException e) {
            // Ignore Error
          }
        }
        catch (XMPPException e) {
          // TODO Auto-generated catch block
          e.printStackTrace();
        }
        Room r = new SimpleRoom(roomName);
        for (int i = 0; i < occupantCount; i++) {
          r.addPlayer(new SimplePlayer("?"));
        }
        ms.addRoom(r);   
        entries.put(moduleKey, ms);
      }
    }
    
    return sortEntriesByModuleName(entries);
  }

  private ServerStatus.ModuleSummary[] sortEntriesByModuleName(
      Map<String,ServerStatus.ModuleSummary> entries) {

    final ServerStatus.ModuleSummary[] e = entries.values().toArray(
      new ServerStatus.ModuleSummary[entries.size()]);
    Arrays.sort(e, new Comparator<ServerStatus.ModuleSummary>() {
      public int compare(ServerStatus.ModuleSummary a, 
                         ServerStatus.ModuleSummary b) {
        return a.getModuleName().compareTo(b.getModuleName());
      }
    });
    return e;
  }
}