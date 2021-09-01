/*
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
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Properties;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.lang3.Range;

import VASSAL.i18n.Resources;
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

  private static final Map<String, Long> timeRanges = new HashMap<>();

  private static final String[] times = {
    Resources.getString(LAST_DAY),
    Resources.getString(LAST_WEEK),
    Resources.getString(LAST_MONTH)
  };

  private final HttpRequestWrapper request;

  public CgiServerStatus() {
    request = new HttpRequestWrapper("https://vassalengine.org/util/"); //$NON-NLS-1$
    timeRanges.put(Resources.getString(LAST_DAY), DAY);
    timeRanges.put(Resources.getString(LAST_WEEK), DAY * 7);
    timeRanges.put(Resources.getString(LAST_MONTH), DAY * 30);
  }

  @Override
  public ServerStatus.ModuleSummary[] getStatus() {
    final HashMap<String, ServerStatus.ModuleSummary> entries =
      new HashMap<>();
    try {
      for (final String s : request.doGet("getCurrentConnections", new Properties())) { //$NON-NLS-1$
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
        catch (final NoSuchElementException e1) {
        }
      }
    }
    // FIXME: review error message
    catch (final IOException e) {
      e.printStackTrace();
    }

    return sortEntriesByModuleName(entries);
  }

  @Override
  public ModuleSummary[] getHistory(String timeRange) {
    final Long l = timeRanges.get(timeRange);
    return l != null ? getHistory(l) : new ModuleSummary[0];
  }

  @Override
  public String[] getSupportedTimeRanges() {
    return times;
  }

  private final SortedMap<Long, List<String[]>> records = new TreeMap<>();
  private final List<Range<Long>> requests = new ArrayList<>();

  private ServerStatus.ModuleSummary[] getHistory(long time) {
    if (time <= 0) return getStatus();

    final long now = System.currentTimeMillis();

    // start with new interval
    final Range<Long> req = Range.between(now - time, now);
    final ArrayList<Range<Long>> toRequest = new ArrayList<>();
    toRequest.add(req);

    // subtract each old interval from new interval
    for (final Range<Long> y : requests) {
      for (final ListIterator<Range<Long>> i = toRequest.listIterator(); i.hasNext();) {
        final Range<Long> x = i.next();

        if (!x.isOverlappedBy(y)) continue; // no overlap, nothing to subtract

        // otherwise, remove x and add what remains after subtracting y
        i.remove();

        final long xl = x.getMinimum();
        final long xr = x.getMaximum();
        final long yl = y.getMinimum();
        final long yr = y.getMaximum();

        if (xl < yl && yl <= xr) i.add(Range.between(xl, yl));
        if (xl <= yr && yr < xr) i.add(Range.between(yr, xr));
      }
    }

    // now toRequest contains the intervals we are missing; request those
    for (final Range<Long> i : toRequest) {
      for (final String s : getInterval(i)) {
        final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '\t');
        try {
          final String moduleName = st.nextToken();
          final String roomName = st.nextToken();
          final String playerName = st.nextToken();
          final Long when = Long.valueOf(st.nextToken());

          final List<String[]> l = records.computeIfAbsent(when, k -> new ArrayList<>());

          l.add(new String[]{ moduleName, roomName, playerName });
        }
        // FIXME: review error message
        catch (final NoSuchElementException | NumberFormatException e) {
          e.printStackTrace();
        }
      }

      requests.add(i);
    }

    // Join intervals to minimize the number we store.
    // Note: This is simple, but quadratic in the number of intervals.
    // For large numbers of intervals, use an interval tree instead.
    for (int i = 0; i < requests.size(); i++) {
      final Range<Long> a = requests.get(i);
      for (int j = i + 1; j < requests.size(); j++) {
        final Range<Long> b = requests.get(j);
        if (a.isOverlappedBy(b)) {
          final long al = a.getMinimum();
          final long ar = a.getMaximum();
          final long bl = b.getMinimum();
          final long br = b.getMaximum();

          requests.set(i, Range.between(Math.min(al, bl),
                                        Math.max(ar, br)));
          requests.remove(j--);
        }
      }
    }

    // pull what we need from the records
    final HashMap<String, ServerStatus.ModuleSummary> entries =
      new HashMap<>();

    for (final List<String[]> l : records.subMap(req.getMinimum(),
                                           req.getMaximum()).values()) {
      for (final String[] r : l) {
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
    }

    return sortEntriesByModuleName(entries);
  }

  private ServerStatus.ModuleSummary[] sortEntriesByModuleName(
      Map<String, ServerStatus.ModuleSummary> entries) {

    final ServerStatus.ModuleSummary[] e = entries.values().toArray(
      new ModuleSummary[0]);
    Arrays.sort(e, (a, b) -> a.getModuleName().compareTo(b.getModuleName()));
    return e;
  }

  private List<String> getInterval(Range<Long> i) {
    final Properties p = new Properties();
    p.setProperty("start", Long.toString(i.getMinimum())); //$NON-NLS-1$
    p.setProperty("end", Long.toString(i.getMaximum())); //$NON-NLS-1$

    try {
      return request.doGet("getConnectionHistory", p); //$NON-NLS-1$
    }
    // FIXME: review error message
    catch (final IOException e) {
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
}
