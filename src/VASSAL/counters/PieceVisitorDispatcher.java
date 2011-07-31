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
package VASSAL.counters;

/**
 * For implementing a modified Visitor pattern on {@link GamePiece}s.
 * Rather than defining accept() methods in the GamePiece subclasses,
 * this class performs the dispatching of visitors.  This allows for easier
 * implementation of custom GamePiece subclasses outside the core engine.
 *
 * A class that wishes to recognize custom GamePiece classes and treat
 * them specially should do so by implementing a sub-interface of
 * PieceVisitor and using a subclass of PieceVisitorDispatcher
 * that recognizes the custom type.
 * @see DeckVisitor
 * @see DeckVisitorDispatcher
 */
public class PieceVisitorDispatcher {
  private PieceVisitor visitor;

  public PieceVisitorDispatcher(PieceVisitor visitor) {
    this.visitor = visitor;
  }

  public Object accept(GamePiece piece) {
    Object value = null;
    if (piece instanceof Stack) {
      value = visitor.visitStack((Stack) piece);
    }
    else {
      value = visitor.visitDefault(piece);
    }
    return value;
  }
}
