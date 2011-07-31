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
 * When executed, adds a {@link PrivateText} to a specified set.
 */
public class SetPrivateTextCommand extends Command {
  private Interface i;
  private PrivateText text;

  public SetPrivateTextCommand(Interface i, PrivateText text) {
    this.i = i;
    this.text = text;
  }

  public PrivateText getPrivateText() {
    return text;
  }

  protected void executeCommand() {
    i.addPrivateText(text);
  }

  protected Command myUndoCommand() {
    return null;
  }

  public static interface Interface {
    void addPrivateText(PrivateText p);
  }
}
