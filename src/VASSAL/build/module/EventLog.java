/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.build.module;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

public class EventLog extends AbstractBuildable
                      implements CommandEncoder, GameComponent {
  public static final String EVENT_LIST = "Events"; //$NON-NLS-1$

  private List<Event> myEvents;
  private List<Event> savedEvents;

  public void addTo(Buildable b) {
    GameModule mod = GameModule.getGameModule();
    mod.addCommandEncoder(this);
    mod.getGameState().addGameComponent(this);
    mod.getPrefs().addOption(new StringConfigurer(EVENT_LIST, null));
    myEvents = new ArrayList<Event>();
    savedEvents = new ArrayList<Event>();

    for (Event e : decodedEvents((String) mod.getPrefs().getValue(EVENT_LIST)))
      myEvents.add(e);
  }

  public void clearSaved() {
    savedEvents.clear();
  }

  public void store(Event e) {
    savedEvents.add(e);
  }

  public void log(Event e) {
    myEvents.add(e);
    GameModule.getGameModule()
              .getPrefs()
              .getOption(EVENT_LIST)
              .setValue(encodedEvents(myEvents));
  }

  public Command decode(String s) {
    if (s.startsWith(EVENT_LIST)) {
      return new StoreEvents(this, s.substring(EVENT_LIST.length()));
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    if (c instanceof StoreEvents) {
      return EVENT_LIST + ((StoreEvents) c).getEvents();
    }
    else {
      return null;
    }
  }

  public void setup(boolean starting) {
    if (!starting) {
      clearSaved();
    }
  }

  public Command getRestoreCommand() {
    return new StoreEvents(this, encodedEvents(savedEvents));
  }

  /**
   * Decodes a <code>String</code> into a sequence of <code>Event</code>s.
   *
   * @param s the event string
   * @return the events represented by the string
   */
  public static Iterable<Event> decodedEvents(final String s) {
    return new Iterable<Event>() {
      public Iterator<Event> iterator() {
        return new Iterator<Event>() {
          private final SequenceEncoder.Decoder se =
            new SequenceEncoder.Decoder(s, '|');

          public boolean hasNext() {
            return se.hasMoreTokens();
          }

          public Event next() {
            final SequenceEncoder.Decoder sub =
              new SequenceEncoder.Decoder(se.nextToken(), ',');
            return new Event(Long.parseLong(sub.nextToken()),
                             sub.nextToken(), sub.nextToken());
          }

          public void remove() {
            throw new UnsupportedOperationException();
          }
        };
      }
    };
  }

  /** Use {@link #decodedEvents()} instead. */
  @Deprecated
  public static Enumeration<Event> decodeEvents(String s) {
    ArrayList<Event> l = new ArrayList<Event>();
    SequenceEncoder.Decoder se = new SequenceEncoder.Decoder(s, '|');
    while (se.hasMoreTokens()) {
      SequenceEncoder.Decoder sub =
        new SequenceEncoder.Decoder(se.nextToken(), ',');
      l.add(new Event(Long.parseLong(sub.nextToken()),
                      sub.nextToken(), sub.nextToken()));
    }
    return Collections.enumeration(l);
  }

  /**
   * Encodes a sequence of <code>Event</code>s into a <code>String</code>.
   *
   * @param events the sequence of events
   * @return the string representing the events
   */
  public static String encodedEvents(Iterable<Event> events) {
    final SequenceEncoder se = new SequenceEncoder('|');
    for (Event e : events) {
      final SequenceEncoder sub = new SequenceEncoder(',');
      sub.append(e.getTime())
         .append(e.getUser())
         .append(e.getAction());
      se.append(sub.getValue());
    }
    return se.getValue();
  }

  /** Use {@link #encodedEvents(Iterable<Event>)} instead. */
  @Deprecated
  public static String encodeEvents(Enumeration<?> e) {
    final SequenceEncoder se = new SequenceEncoder('|');
    while (e.hasMoreElements()) {
      final Event evt = (Event) e.nextElement();
      final SequenceEncoder sub = new SequenceEncoder(',');
      sub.append(evt.getTime())
         .append(evt.getUser())
         .append(evt.getAction());
      se.append(sub.getValue());
    }
    return se.getValue();
  }

  public static class Event {
    private long time;
    private String user;
    private String action;

    public Event(long time, String user, String action) {
      this.time = time;
      this.user = user;
      this.action = action;
    }

    public long getTime() {
      return time;
    }

    public String getUser() {
      return user;
    }

    public String getAction() {
      return action;
    }
  }

  public static class StoreEvents extends Command {
    private EventLog log;
    private String events;

    public StoreEvents(EventLog log, String events) {
      this.log = log;
      this.events = events;
    }

    public String getEvents() {
      return events;
    }

    public void executeCommand() {
      log.clearSaved();
      for (Event e : decodedEvents(events)) log.store(e);
    }

    public Command myUndoCommand() {
      return null;
    }
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String name, Object value) {
  }

  public String getAttributeValueString(String name) {
    return null;
  }
}
