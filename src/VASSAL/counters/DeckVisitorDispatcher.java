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
 * For handling Visitors that are aware of {@link Deck} types
 */
public class DeckVisitorDispatcher extends PieceVisitorDispatcher {
  private DeckVisitor visitor;

  public DeckVisitorDispatcher(DeckVisitor visitor) {
    super(visitor);
    this.visitor = visitor;
  }

  public Object accept(GamePiece piece) {
    if (piece instanceof Deck) {
      return visitor.visitDeck((Deck)piece);
    }
    else {
      return super.accept(piece);
    }
  }
}
