/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;
import VASSAL.i18n.Resources;
import VASSAL.tools.HashCode;
import VASSAL.tools.SequenceEncoder;

/**
 * Queries a known URL to get historical status of the chat room server.
 *
 * @author rkinney
 */
public class CgiServerStatus implements ServerStatus {
  private static final long DAY = 24L * 3600L * 1000L;
  
  public static final String LAST_DAY = "Server.last_24_hours"; //$NON-NLS-1$
  public static final String LAST_WEEK = "Server.last_week"; //$NON-NLS-1$
  public static final String LAST_MONTH = "Server.last_month"; //$NON-NLS-1$
  
  private static final Map<String,Long> timeRanges = new HashMap<String,Long>();
  
  private static final String[] times = new String[]{
    Resources.getString(LAST_DAY),
    Resources.getString(LAST_WEEK),
    Resources.getString(LAST_MONTH)
  };

  private HttpRequestWrapper request;

  public CgiServerStatus() {
    request = new HttpRequestWrapper("http://www.vassalengine.org/util/"); //$NON-NLS-1$
    timeRanges.put(Resources.getString(LAST_DAY), DAY);
    timeRanges.put(Resources.getString(LAST_WEEK), DAY * 7);
    timeRanges.put(Resources.getString(LAST_MONTH), DAY * 30);
  }

  public ServerStatus.ModuleSummary[] getStatus() {
    final HashMap<String,ServerStatus.ModuleSummary> entries =
      new HashMap<String,ServerStatus.ModuleSummary>();
    try {
      for (String s : request.doGet("getCurrentConnections", new Properties())) { //$NON-NLS-1$
        final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
        try {
          final String moduleName = st.nextToken();
          final String roomName = st.nextToken();
          final String playerName = st.nextToken();
          final ServerStatus.ModuleSummary entry = entries.get(moduleName);
          if (entry == null) {
            entries.put(moduleName,
                        createEntry(moduleName, roomName, playerName));
          }
          else {
            updateEntry(entry, roomName, playerName);
          }
        }
        // FIXME: review error message
        catch (NoSuchElementException e1) {
        }
      }
    }
    // FIXME: review error message
    catch (IOException e) {
      e.printStackTrace();
    }

    return sortEntriesByModuleName(entries);
  }

  public ModuleSummary[] getHistory(String timeRange) {
    final Long l = timeRanges.get(timeRange);
    return l != null ? getHistory(l.longValue()) : new ModuleSummary[0];
  }

  public String[] getSupportedTimeRanges() {
    return times;
  }

  private SortedMap<Long,String[]> records = new TreeMap<Long,String[]>();
  private List<Interval> requests = new ArrayList<Interval>();

  private ServerStatus.ModuleSummary[] getHistory(long time) {
    if (time <= 0) return getStatus();

    final long now = System.currentTimeMillis() +
      TimeZone.getDefault().getOffset(Calendar.ERA,
                                      Calendar.YEAR,
                                      Calendar.MONTH,
                                      Calendar.DAY_OF_YEAR,
                                      Calendar.DAY_OF_WEEK,
                                      Calendar.MILLISECOND);

    // start with new interval
    final Interval req = new Interval(now - time, now);
    final ArrayList<Interval> toRequest = new ArrayList<Interval>();
    toRequest.add(req);

    // subtract each old interval from new interval
    for (Interval y : requests) {
      for (ListIterator<Interval> i = toRequest.listIterator(); i.hasNext();) {
        final Interval x = i.next();
        
        if (!x.intersects(y)) continue;   // no overlap, nothing to subtract
    
        // otherwise, remove x and add what remains after subtracting y
        i.remove();
        if (x.l < y.l && y.l <= x.r) i.add(new Interval(x.l, y.l));
        if (x.l <= y.r && y.r < x.r) i.add(new Interval(y.r, x.r));
      }
    }
       
    // now toRequest contains the intervals we are missing; request those
    for (Interval i : toRequest) {
      for (String s : getInterval(i)) {
        final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
        try {
          final String moduleName = st.nextToken();
          final String roomName = st.nextToken();
          final String playerName = st.nextToken();
          final Long when = Long.valueOf(st.nextToken());
          records.put(when, new String[]{ moduleName, roomName, playerName });
        }
        // FIXME: review error message
        catch (NoSuchElementException e) {
          e.printStackTrace();
        }
        // FIXME: review error message
        catch (NumberFormatException e) {
          e.printStackTrace();
        }
      }
  
      requests.add(i);
    }

    // Join intervals to minimize the number we store.
    // Note: This is simple, but quadratic in the number of intervals.
    // For large numbers of intervals, use an interval tree instead.
    for (int i = 0; i < requests.size(); i++) {
      final Interval a = requests.get(i);
      for (int j = i+1; j < requests.size(); j++) {
        final Interval b = requests.get(j);
        if (a.intersects(b)) {
          requests.set(i, new Interval(Math.min(a.l, b.l), Math.max(a.r, b.r)));
          requests.remove(j--);
        }
      }
    }

    // pull what we need from the records
    final HashMap<String,ServerStatus.ModuleSummary> entries =
      new HashMap<String,ServerStatus.ModuleSummary>();

    for (Long when : records.subMap(req.l, req.r).keySet()) {
      final String[] r = records.get(when);
      final String moduleName = r[0]; 
      final String roomName = r[1];
      final String playerName = r[2];

      final ServerStatus.ModuleSummary entry = entries.get(moduleName);
      if (entry == null) {
        entries.put(moduleName,
                    createEntry(moduleName, roomName, playerName));
      }
      else {
        updateEntry(entry, roomName, playerName);
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

  private List<String> getInterval(Interval i) {
    final Properties p = new Properties();
    p.setProperty("start", Long.toString(i.l));
    p.setProperty("end", Long.toString(i.r));

    try {
      return request.doGet("getConnectionHistory", p);
    }
    // FIXME: review error message
    catch (IOException e) {
      e.printStackTrace();
    }

    return null;
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
    final SimpleRoom r = new SimpleRoom(roomName);
    r.setPlayers(new Player[]{new SimplePlayer(playerName)});
    return new ServerStatus.ModuleSummary(moduleName, new Room[]{r});
  }

  /**
   * A closed interval.
   *
   * @author Joel Uckelman
   * @since 3.1.0
   */
  private static class Interval {
    public final long l;
    public final long r;

    public Interval(long left, long right) {
      if (right < left) throw new IllegalArgumentException();

      l = left;
      r = right;
    }

    public boolean intersects(Interval i) {
      return (l <= i.l && i.l <= r) ||
             (i.l <= l && l <= i.r);
    }

    public boolean equals(Object o) {
      if (!(o instanceof Interval)) return false;
      final Interval i = (Interval) o;
      return i.l == l && i.r == r;
    }

    public int hashCode() {
      return HashCode.hash(l) ^ HashCode.hash(r);
    }

    public String toString() {
      return "[" + l + "," + r + "]";
    }
  }
}
