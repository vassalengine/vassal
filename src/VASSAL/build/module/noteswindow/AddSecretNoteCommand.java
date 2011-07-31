package VASSAL.build.module.noteswindow;

import VASSAL.command.Command;

/*
 * $Id$
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
 * When executed, adds a {@link SecretNote} to a specified List.
 */
public class AddSecretNoteCommand extends Command {
  private Interface i;
  private SecretNote note;

  public AddSecretNoteCommand(Interface i, SecretNote note) {
    this.i = i;
    this.note = note;
  }

  public SecretNote getNote() {
    return note;
  }

  protected void executeCommand() {
    i.addSecretNote(note);
  }

  protected Command myUndoCommand() {
    return null;
  }

  public static interface Interface {
    void addSecretNote(SecretNote note);
  }
}
