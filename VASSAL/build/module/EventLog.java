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

import java.util.Enumeration;
import java.util.Vector;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

public class EventLog extends AbstractBuildable implements CommandEncoder, GameComponent {
  public static final String EVENT_LIST = "Events";

  private Vector myEvents;
  private Vector savedEvents;

  public void addTo(Buildable b) {
    GameModule mod = GameModule.getGameModule();
    mod.addCommandEncoder(this);
    mod.getGameState().addGameComponent(this);
    mod.getPrefs().addOption(new StringConfigurer(EVENT_LIST, null));
    myEvents = new Vector();
    savedEvents = new Vector();
    for (Enumeration e = decodeEvents((String) mod.getPrefs().getValue(EVENT_LIST));
         e.hasMoreElements();) {
      myEvents.addElement(e.nextElement());
    }
  }

  public void clearSaved() {
    savedEvents.removeAllElements();
  }

  public void store(Event e) {
    savedEvents.addElement(e);
  }

  public void log(Event e) {
    myEvents.addElement(e);
    GameModule.getGameModule().getPrefs().getOption(EVENT_LIST).setValue(encodeEvents(myEvents.elements()));
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
    return new StoreEvents(this, encodeEvents(savedEvents.elements()));
  }

  public static Enumeration decodeEvents(String s) {
    Vector v = new Vector();
    SequenceEncoder.Decoder se = new SequenceEncoder.Decoder(s, '|');
    while (se.hasMoreTokens()) {
      SequenceEncoder.Decoder sub = new SequenceEncoder.Decoder(se.nextToken(), ',');
      v.addElement(new Event(Long.parseLong(sub.nextToken()),
                             sub.nextToken(), sub.nextToken()));
    }
    return v.elements();
  }

  public static String encodeEvents(Enumeration e) {
    SequenceEncoder se = new SequenceEncoder('|');
    while (e.hasMoreElements()) {
      Event evt = (Event) e.nextElement();
      SequenceEncoder sub = new SequenceEncoder(',');
      sub.append("" + evt.getTime())
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
      for (Enumeration e = decodeEvents(events);
           e.hasMoreElements();) {
        log.store((Event) e.nextElement());
      }
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
