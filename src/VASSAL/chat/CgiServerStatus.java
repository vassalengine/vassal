/*
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
package VASSAL.chat;

import java.io.IOException;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.TimeZone;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

/**
 * Queries a known URL to get historical status of the chat room server
 * @author rkinney
 *
 */
public class CgiServerStatus implements ServerStatus {
  private static long DAY = 24L * 3600L * 1000L;
  
  public static final String LAST_DAY = "Server.last_24_hours"; //$NON-NLS-1$
  public static final String LAST_WEEK = "Server.last_week"; //$NON-NLS-1$
  public static final String LAST_MONTH = "Server.last_month"; //$NON-NLS-1$
  
  private Map<String,Long> timeRanges = new HashMap<String,Long>();
  
  private String[] times = new String[]{
    Resources.getString(LAST_DAY),
    Resources.getString(LAST_WEEK),
    Resources.getString(LAST_MONTH)
  };

  private HttpRequestWrapper request;
  private List<String> cachedQuery;

  public CgiServerStatus() {
    request = new HttpRequestWrapper("http://www.vassalengine.org/util/"); //$NON-NLS-1$
    timeRanges.put(Resources.getString(LAST_DAY), DAY);
    timeRanges.put(Resources.getString(LAST_WEEK), DAY * 7);
    timeRanges.put(Resources.getString(LAST_MONTH), DAY * 30);
  }

  public ServerStatus.ModuleSummary[] getStatus() {
    HashMap<String,ServerStatus.ModuleSummary> entries =
      new HashMap<String,ServerStatus.ModuleSummary>();
    try {
      for (String s : request.doGet("getCurrentConnections", new Properties())) { //$NON-NLS-1$
        SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
        try {
          String moduleName = st.nextToken();
          String roomName = st.nextToken();
          String playerName = st.nextToken();
          ServerStatus.ModuleSummary entry = entries.get(moduleName);
          if (entry == null) {
            entries.put(moduleName,
                        createEntry(moduleName, roomName, playerName));
          }
          else {
            updateEntry(entry, roomName, playerName);
          }
        }
        catch (NoSuchElementException e1) {
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    cachedQuery = null;
    retrieveHistory();
    ServerStatus.ModuleSummary[] entry =
      entries.values().toArray(new ServerStatus.ModuleSummary[entries.size()]);
    return entry;
  }

  public ModuleSummary[] getHistory(String timeRange) {
    Long l = timeRanges.get(timeRange);
    if (l != null) {
      return getHistory(l.longValue());
    }
    else {
      return new ModuleSummary[0];
    }
  }

  public String[] getSupportedTimeRanges() {
    return times;
  }

  private ServerStatus.ModuleSummary[] getHistory(long time) {
    if (time <= 0) {
      return getStatus();
    }
    long now = System.currentTimeMillis();
    now += TimeZone.getDefault().getOffset(Calendar.ERA,
                                           Calendar.YEAR,
                                           Calendar.MONTH,
                                           Calendar.DAY_OF_YEAR,
                                           Calendar.DAY_OF_WEEK,
                                           Calendar.MILLISECOND);
    HashMap<String,ServerStatus.ModuleSummary> entries =
      new HashMap<String,ServerStatus.ModuleSummary>();
    for (String s : retrieveHistory()) {
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
      try {
        String moduleName = st.nextToken();
        String roomName = st.nextToken();
        String playerName = st.nextToken();
        long when = Long.parseLong(st.nextToken());
        if (now - when <= time) {
          ServerStatus.ModuleSummary entry = entries.get(moduleName);
          if (entry == null) {
            entries.put(moduleName,
                        createEntry(moduleName, roomName, playerName));
          }
          else {
            updateEntry(entry, roomName, playerName);
          }
        }
      }
      catch (NoSuchElementException e1) {
      }
      catch (NumberFormatException e2) {
      }
    }
    ServerStatus.ModuleSummary[] entry = 
      entries.values().toArray(new ServerStatus.ModuleSummary[entries.size()]);
    return entry;
  }

  private List<String> retrieveHistory() {
    if (cachedQuery == null) {
      try {
        cachedQuery = request.doGet("getConnectionHistory", new Properties()); //$NON-NLS-1$
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    return cachedQuery;
  }

  private ServerStatus.ModuleSummary updateEntry(
    ServerStatus.ModuleSummary entry, String roomName, String playerName) {

    SimpleRoom existingRoom = entry.getRoom(roomName);
    if (existingRoom == null) {
      existingRoom = new SimpleRoom(roomName);
      existingRoom.setPlayers(new Player[]{new SimplePlayer(playerName)});
      entry.addRoom(existingRoom);
    }
    else {
      existingRoom.addPlayer(new SimplePlayer(playerName));
    }
    return entry;
  }

  private ServerStatus.ModuleSummary createEntry(String moduleName,
                                                 String roomName,
                                                 String playerName) {
    SimpleRoom r = new SimpleRoom(roomName);
    r.setPlayers(new Player[]{new SimplePlayer(playerName)});
    return new ServerStatus.ModuleSummary(moduleName, new Room[]{r});
  }
}
