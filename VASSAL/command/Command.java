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
package VASSAL.command;

import VASSAL.build.GameModule;

/**
 * Command is an abstract class that does something.  Any action that
 * takes place during a game should be encapsulated in a Command
 * object.  When performing actions during a game, corresponding
 * Commands will be logged in the current logfile and/or sent to other
 * players on the server.
 *
 * Commands can be strung together into compound commands with the
 * {@link #append} method.
 *
 * @see CommandEncoder */
public abstract class Command {
  private Command seq[] = new Command[0];
  private Command undo;

  public Command() {
  }

  public Command[] getSubCommands() {
    return seq;
  }

  /**
   * Execute this command by first invoking {@link #executeCommand},
   * then invoking {@link #execute} recursively on all subcommands.  */
  public void execute() {
    try {
      executeCommand();
    }
    catch (Exception ex) {
      Command[] oldSeq = seq;
      stripSubCommands();
      reportException(this, ex);
      seq = oldSeq;
    }
    for (int i = 0; i < seq.length; ++i) {
      try {
        seq[i].execute();
      }
      catch (Exception ex) {
        reportException(seq[i], ex);
      }
    }
  }

  private void reportException(Command c, Exception ex) {
    String s = GameModule.getGameModule() == null ?
      c.toString() : GameModule.getGameModule().encode(c);
    System.err.println("Unable to execute " + s);
    ex.printStackTrace();
  }

  /**
   * Perform the action which this Command represents
   */
  protected abstract void executeCommand();

  /**
   * If the action can be undone, return a Command that performs the
   * inverse action.  The Command returned should only undo {@link
   * #executeCommand}, not the actions of subcommands */
  protected abstract Command myUndoCommand();

  /**
   * Remove all subcommands.
   */
  public void stripSubCommands() {
    seq = new Command[0];
  }

  /**
   * @return true if this command does nothing
   */
  public boolean isNull() {
    return false;
  }

  /**
   *
   * @return true if this command should be stored in a logfile
   */
  public boolean isLoggable() {
    return !isNull();
  }

  protected boolean hasNullSubcommands() {
    for (int i = 0; i < seq.length; ++i) {
      if (!seq[i].isNull()) {
        return false;
      }
    }
    return true;
  }

  public String toString() {
    String s = getClass().getName();
    s = s.substring(s.lastIndexOf(".") + 1);
    String details = getDetails();
    if (details != null) {
      s += "[" + details+"]";
    }
    for (int i = 0; i < seq.length; ++i) {
      s += "+" + seq[i].toString();
    }
    return s;
  }
  
  /** Detailed information for toString() */
  public String getDetails() {
    return null;
  }

  /**
   * Append a subcommand to this Command.
   */
  public Command append(Command c) {
    Command retval = this;
    if (c != null && !c.isNull()) {
      if (isNull()) {
        retval = c;
      }
      Command[] oldSeq = seq;
      seq = new Command[seq.length + 1];
      System.arraycopy(oldSeq, 0, seq, 0, seq.length - 1);
      seq[seq.length - 1] = c;
    }
    return retval;
  }

  /**
   * @return a Command that undoes not only this Command's action,
   * but also the actions of all its subcommands.  */
  public Command getUndoCommand() {
    if (undo == null) {
      undo = new NullCommand();
      for (int i = seq.length - 1; i >= 0; --i) {
        undo = undo.append(seq[i].getUndoCommand());
      }
      undo = undo.append(myUndoCommand());
    }
    return undo;
  }
}
