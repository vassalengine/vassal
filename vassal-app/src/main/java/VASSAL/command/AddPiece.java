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

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.map.HighlightLastMoved;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceIterator;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;

/**
 * This Command adds a {@link GamePiece} to a game.  Its undo
 * Command is {@link RemovePiece}.
 */
public class AddPiece extends Command {
  private Command undo;
  private final GamePiece target;
  private final String state;

  public AddPiece(GamePiece p) {
    this(p, p.getState());
  }

  public AddPiece(GamePiece p, String state) {
    target = p;
    this.state = state;
  }

  /**
   * Adds a piece by invoking {@link GameState#addPiece}, followed by
   * {@link GamePiece#setState}
   */
  @Override
  protected void executeCommand() {
    if (target != null) {
      GameModule.getGameModule().getGameState().addPiece(target);
      target.setState(state);
      if (target.getMap() != null) {
        HighlightLastMoved.setLastMoved(target);
        if (GlobalOptions.getInstance().centerOnOpponentsMove()
            && !Boolean.TRUE.equals(target.getProperty(Properties.INVISIBLE_TO_ME))) {

          // Do not centre on a Stack unless it has at least one visible piece
          if (target instanceof Stack && !((Stack) target).asList().stream().anyMatch(PieceIterator.VISIBLE)) {
            return;
          }

          target.getMap().ensureVisible(target.getMap().selectionBoundsOf(target));
          target.getMap().repaint();
        }
      }
    }
  }

  @Override
  protected Command myUndoCommand() {
    if (undo == null) {
      undo = new RemovePiece(target);
    }
    return undo;
  }

  public GamePiece getTarget() {
    return target;
  }

  public String getState() {
    return state;
  }
}
