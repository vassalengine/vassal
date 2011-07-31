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
import VASSAL.build.module.GlobalOptions;
import VASSAL.counters.BoundsTracker;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.counters.StateMergeable;

/**
 * This Command changes the state of a {@link GamePiece}.  Its undo
 * Command is another ChangePiece with the new and old states
 * reversed.  */
public class ChangePiece extends Command {
  protected String newState, oldState;
  private String id;
  private Command undo;

  /**
   * @param id the id of the {@link GamePiece} to change
   * @param oldState the previous state of the piece
   * @param newState the new state of the piece
   */
  public ChangePiece(String id, String oldState, String newState) {
    this.id = id;
    this.newState = newState;
    this.oldState = oldState;
  }

  /**
   * When using this constructor, the previous state will be set
   * to that of the piece when this Command is executed.
   * @deprecated
   */
  @Deprecated public ChangePiece(String id, String newState) {
    this(id, null, newState);
  }

  /**
   * Changes the state of a {@link GamePiece} by invoking {@link GamePiece#setState}
   */
  protected void executeCommand() {
    GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(id);
    if (target != null) {
      BoundsTracker bounds = new BoundsTracker();
      bounds.addPiece(target);
      if (oldState != null) {
        if (target instanceof StateMergeable) {
          ((StateMergeable) target).mergeState(newState, oldState);
        }
        else {
          target.setState(newState);
        }
      }
      else {
        oldState = target.getState();
        target.setState(newState);
      }
      bounds.addPiece(target);
      bounds.repaint();
      if (target.getMap() != null
        && GlobalOptions.getInstance().centerOnOpponentsMove()
        && !Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_ME))) {
        target.getMap().ensureVisible(target.getMap().selectionBoundsOf(target));
      }
    }
  }

  public Command append(Command c) {
    Command last = this;
    Command[] sub = getSubCommands();
    if (sub.length > 0) {
      last = sub[sub.length - 1];
    }
    if (c instanceof ChangePiece
      && last instanceof ChangePiece
      && ((ChangePiece) c).id != null
      && ((ChangePiece) c).id.equals(((ChangePiece) last).id)
      && ((ChangePiece) c).newState != null) {
      ((ChangePiece) last).newState = ((ChangePiece) c).newState;
      sub = c.getSubCommands();
      for (int i = 0; i < sub.length; ++i) {
        append(sub[i]);
      }
      return this;
    }
    else {
      return super.append(c);
    }
  }

  protected Command myUndoCommand() {
    if (undo == null && oldState != null) {
      undo = new ChangePiece(id, newState, oldState);
    }
    return undo;
  }

  public String getId() {
    return id;
  }

  public String getNewState() {
    return newState;
  }

  public String getOldState() {
    return oldState;
  }

  public boolean isNull() {
    return newState.equals(oldState) && isAtomic();
  }

  public String getDetails() {
    return "id="+id+",oldState="+oldState+",newState="+newState;
  }
}
