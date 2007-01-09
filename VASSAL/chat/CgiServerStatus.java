package VASSAL.chat;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.TimeZone;
import VASSAL.tools.SequenceEncoder;

/**
 * Queries a known URL to get historical status of the chat room server
 * @author rkinney
 *
 */
public class CgiServerStatus implements ServerStatus {
  private static long DAY = 24L * 3600L * 1000L;
  
  public static final String LAST_DAY = "Last 24 hours";
  public static final String LAST_WEEK = "Last Week";
  public static final String LAST_MONTH = "Last Month";
  
  private Map timeRanges = new HashMap();
  
  private String[] times = new String[]{LAST_DAY,LAST_WEEK,LAST_MONTH};

  private HttpRequestWrapper request;
  private List cachedQuery;

  public CgiServerStatus() {
    request = new HttpRequestWrapper("http://www.vassalengine.org/util/");
    timeRanges.put(LAST_DAY,new Long(DAY));
    timeRanges.put(LAST_WEEK,new Long(DAY * 7));
    timeRanges.put(LAST_MONTH,new Long(DAY * 30));
  }

  public ServerStatus.ModuleSummary[] getStatus() {
    Map entries = new HashMap();
    try {
      for (Enumeration e = request.doGet("getCurrentConnections", new Properties()); e.hasMoreElements();) {
        String s = (String) e.nextElement();
        SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
        try {
          String moduleName = st.nextToken();
          String roomName = st.nextToken();
          String playerName = st.nextToken();
          ServerStatus.ModuleSummary entry = (ServerStatus.ModuleSummary) entries.get(moduleName);
          if (entry == null) {
            entries.put(moduleName, createEntry(moduleName, roomName, playerName));
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
    ServerStatus.ModuleSummary[] entry = (ServerStatus.ModuleSummary[]) entries.values().toArray(new ServerStatus.ModuleSummary[entries.size()]);
    return entry;
  }

  public ModuleSummary[] getHistory(String timeRange) {
    Long l = (Long) timeRanges.get(timeRange);
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
    now += TimeZone.getDefault().getOffset(Calendar.ERA, Calendar.YEAR, Calendar.MONTH, Calendar.DAY_OF_YEAR, Calendar.DAY_OF_WEEK, Calendar.MILLISECOND);
    Map entries = new HashMap();
    for (Iterator it = retrieveHistory().iterator(); it.hasNext();) {
      String s = (String) it.next();
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
      try {
        String moduleName = st.nextToken();
        String roomName = st.nextToken();
        String playerName = st.nextToken();
        long when = Long.parseLong(st.nextToken());
        if (now - when <= time) {
          ServerStatus.ModuleSummary entry = (ServerStatus.ModuleSummary) entries.get(moduleName);
          if (entry == null) {
            entries.put(moduleName,createEntry(moduleName, roomName, playerName));
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
    ServerStatus.ModuleSummary[] entry = (ServerStatus.ModuleSummary[]) entries.values().toArray(new ServerStatus.ModuleSummary[entries.size()]);
    return entry;
  }

  private List retrieveHistory() {
    if (cachedQuery == null) {
      cachedQuery = new ArrayList();
      try {
        for (Enumeration e = request.doGet("getConnectionHistory", new Properties()); e.hasMoreElements();) {
          cachedQuery.add(e.nextElement());
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    return cachedQuery;
  }

  private ServerStatus.ModuleSummary updateEntry(ServerStatus.ModuleSummary entry, String roomName, String playerName) {
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

  private ServerStatus.ModuleSummary createEntry(String moduleName, String roomName, String playerName) {
    SimpleRoom r = new SimpleRoom(roomName);
    r.setPlayers(new Player[]{new SimplePlayer(playerName)});
    return new ServerStatus.ModuleSummary(moduleName, new Room[]{r});
  }
}
