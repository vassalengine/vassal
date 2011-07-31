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

import java.awt.Rectangle;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.HighlightLastMoved;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.Stack;

/**
 * This Command removed a {@link GamePiece} from a game.  Its undo
 * Command is {@link AddPiece}.  */
public class RemovePiece extends Command {
  private Command undo = null;
  private GamePiece target;
  private String id;

  public RemovePiece(GamePiece p) {
    target = p;
  }

  public RemovePiece(String id) {
    this.id = id;
  }

  /**
   * Removes a piece by invoking {@link Map#removePiece} if the
   * piece belongs to a {@link Map}, followed by {@link
   * GameState#removePiece}.
   */
  protected void executeCommand() {
    if (target == null) {
      target = GameModule.getGameModule().getGameState().getPieceForId(id);
      if (target == null) return;
    }

    undo = new AddPiece(target, target.getState());

    Rectangle r = null;
    final Map m = target.getMap();
    final Stack parent = target.getParent();

    // Highlight the stack the piece was removed from - Ben
    HighlightLastMoved.setLastMoved(target);

    if (m != null) {
      r = parent == null ?  m.boundingBoxOf(target) : m.boundingBoxOf(parent);
      m.removePiece(target);
      target.setMap(null);
    }

    if (parent != null) {
      final String stateWithPiece = parent.getState();
      parent.remove(target);
      undo = undo.append(
        new ChangePiece(parent.getId(),parent.getState(), stateWithPiece));
      target.setParent(null);
    }

    if (m != null) {
      m.repaint(r);
    }

    GameModule.getGameModule().getGameState().removePiece(target.getId());
    KeyBuffer.getBuffer().remove(target);
  }

  protected Command myUndoCommand() {
    if (undo == null && target != null) {
      undo = new AddPiece(target);
    }
    return undo;
  }

  public GamePiece getTarget() {
    return target;
  }

  public String getId() {
    return target == null ? id : target.getId();
  }
}
