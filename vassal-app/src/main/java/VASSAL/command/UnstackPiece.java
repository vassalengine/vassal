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

import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;

/**
 * This Command removes a {@link GamePiece} from a stack.  Its undo
 * Command is {@link StackPiece}.  */
public class UnstackPiece extends Command {
  private Command undo = null;
  private Stack stack;
  private GamePiece piece;

  public UnstackPiece(Stack s, GamePiece p) {
    stack = s;
    piece = p;
  }

  /**
   * Removes the piece form the stack;
   */
  @Override
  protected void executeCommand() {
    undo = new StackPiece(stack, piece);
    stack.remove(piece);
  }

  @Override
  protected Command myUndoCommand() {
    if (undo == null && stack != null && piece != null) {
      undo = new StackPiece(stack, piece);
    }
    return undo;
  }
}
