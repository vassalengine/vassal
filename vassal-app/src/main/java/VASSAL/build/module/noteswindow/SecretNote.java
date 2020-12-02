package VASSAL.build.module.noteswindow;

import java.util.Date;
import java.util.Objects;

import VASSAL.build.GameModule;
import VASSAL.build.module.PlayerRoster;

/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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

/**
 * Represents a text note with an owner and hidden/revealed status.
 * This is an immutable object
 */
public class SecretNote {
  private final String owner;    // Owner's Password
  private final String name;     // Name of Note
  private final String text;     // Text of Note
  private final boolean hidden;  // Is note still hidden?
  private final Date date;       // Date/Time stamp
  private final String handle;   // Owner's handle

  public SecretNote(String name, String owner, String text, boolean hidden) {
    this(name, owner, text, hidden, new Date(), findHandle());
  }

  private static String findHandle() {
    return PlayerRoster.isActive() && PlayerRoster.getMySide() != null ?
      PlayerRoster.getMySide() :
      (String) GameModule.getGameModule().getPrefs().getOption(GameModule.REAL_NAME).getValue();
  }

  public SecretNote(String name, String owner, String text, boolean hidden, Date created, String id) {
    this.name = name;
    this.owner = owner;
    this.text = text;
    this.hidden = hidden;
    this.date = created;
    this.handle = id;
  }

  public boolean isHidden() {
    return hidden;
  }

  public String getName() {
    return name;
  }

  public String getOwner() {
    return owner;
  }

  public Date getDate() {
    return date;
  }

  public String getHandle() {
    return handle;
  }

  /**
   * Two SecretNotes with the same owner and name are considered equal
   * @param o
   * @return
   */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof SecretNote)) return false;

    final SecretNote secretNote = (SecretNote) o;
    return Objects.equals(name, secretNote.name) &&
           Objects.equals(owner, secretNote.owner);
  }

  @Override
  public int hashCode() {
    return Objects.hash(owner, name);
  }

  public String getText() {
    return text;
  }
}
