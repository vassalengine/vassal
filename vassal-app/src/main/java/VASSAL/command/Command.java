/*
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

import VASSAL.tools.ProblemDialog;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ThrowableUtils;
import VASSAL.build.module.GameComponent;

/**
 * A Command represents an action that needs to be transmitted from one client to another - any action that could
 * change the game state of a multiplayer game should be encapsulated in a Command object. When performing actions
 * during the game, corresponding Commands will be logged in the correct logfile and/or sent to other players through
 * the server.
 *
 * {@link CommandEncoder}s then serialise ({@link CommandEncoder#encode}) and deserialise ({@link CommandEncoder#decode})
 * Commands to and from an ascii based representation. Commands are encoded by the generated client prior to being sent
 * across network or saved in a log or save file. Commands are decoded by the receiving client on receipt from the network
 * or on reading from a log or save file. Save game creation is a special case where every {@link GameComponent} is asked to
 * generate a Command that when executed will cause itself to be recreated in its present state.
 *
 * The {@link #execute} method implements the execution of the command and is called by the receiving client
 * after building the Command using <code>decode</code>. The <code>execute</code> method is sometimes called on the generating client but does
 * not need to be if the Command is being created to encapsulate something that has already happened on the generating client.
 *
 * Commands can be strung together into compound commands with the {@link #append} method. Although Commands can be linked into
 * compound commands this way, each {@link CommandEncoder} need only handle single (not compound) commands.
 */
public abstract class Command {
  private List<Command> seq = new LinkedList<>();
  private Command undo;

  public Command[] getSubCommands() {
    return seq.toArray(new Command[0]);
  }

  /**
   * Execute this command by first invoking {@link #executeCommand}, then
   * invoking itself recursively on all subcommands.
   */
  public void execute() {
    try {
      executeCommand();
    }
    catch (Throwable t) {
      handleFailure(t);

      final List<Command> oldSeq = seq;
      stripSubCommands();
      seq = oldSeq;
    }

    for (final Command cmd : seq) {
      try {
        cmd.execute();
      }
      catch (Throwable t) {
        handleFailure(t);
      }
    }
  }

  private void handleFailure(Throwable t) {
    // find and rethrow causes which are not bugs
    ThrowableUtils.throwRecent(OutOfMemoryError.class, t);

    if (t instanceof Error) {
      // some unusual problem occurred
      throw (Error) t;
    }
    else {
      // report the bug here
      ErrorDialog.bug(t);
    }
  }

  /**
   * Perform the action which this Command represents
   */
  protected abstract void executeCommand();

  /**
   * If the action can be undone, return a Command that performs the
   * inverse action. The Command returned should only undo
   * {@link #executeCommand}, not the actions of subcommands
   */
  protected abstract Command myUndoCommand();

  /**
   * Remove all subcommands.
   */
  public void stripSubCommands() {
    seq = new LinkedList<>();
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

  /**
   * @deprecated Use {@link #isAtomic()}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected boolean hasNullSubcommands() {
    ProblemDialog.showDeprecated("2020-08-06");
    return isAtomic();
  }

  /**
   * Return true if this command has no sub-commands attached to it
   * (other than null commands).
   *
   * @return True if this command has no sub-commands
   */
  protected boolean isAtomic() {
    for (final Command c : seq) {
      if (!c.isNull()) {
        return false;
      }
    }
    return true;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder(getClass().getSimpleName());
    final String details = getDetails();
    if (details != null) sb.append('[').append(details).append(']');

    for (final Command c : seq) sb.append('+').append(c);

    return sb.toString();
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
      seq.add(c);
    }
    return retval;
  }

  /**
   * @return a Command that undoes not only this Command's action, but also
   * the actions of all its subcommands.
   */
  public Command getUndoCommand() {
    if (undo == null) {
      undo = new NullCommand();
      for (final ListIterator<Command> i = seq.listIterator(seq.size());
           i.hasPrevious(); ) {
        undo = undo.append(i.previous().getUndoCommand());
      }
      undo = undo.append(myUndoCommand());
    }
    return undo;
  }

  /**
   * Return true if this Command is a NullCommand or only contains
   * Null Commands or commands of the same type as target
   * @param target Class to inspect for
   * @return true if this command contains only non-null Commands target class Commands
   */
  public boolean isNullOrContainsOnly(Class<?> target) {
    if (this instanceof NullCommand || target.isInstance(this)) {
      for (final Command c : seq) {
        if (c != null && !c.isNullOrContainsOnly(target)) {
          return false;
        }
      }
      return true;
    }
    return false;
  }
}
