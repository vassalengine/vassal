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
package VASSAL.counters;

import java.awt.Point;
import java.awt.Shape;
import java.util.Iterator;

import VASSAL.build.module.Map;

import static VASSAL.counters.Mat.MAT_NAME;

/**
 * This interface defines selection criteria for finding a GamePiece in a Map
 */
@FunctionalInterface
public interface PieceFinder {
  /** Return the argument GamePiece (or one of its children if a Stack) found at the given point on the given Map */
  GamePiece select(Map map, GamePiece piece, Point pt);

  /** Return a Stack overlapping the given point */
  PieceFinder STACK_ONLY = new StackOnly();

  /**
   * If a Stack overlaps the given point, return the piece containing that point if expanded,
   * or the top piece if not expanded.
   * */
  PieceFinder PIECE_IN_STACK = new PieceInStack();

  /**
   * If a Stack overlaps the given point, return the piece containing that point if expanded,
   * or the top piece if not expanded.
   * If a Deck is found instead, return the deck.
   */
  PieceFinder DECK_OR_PIECE_IN_STACK = new DeckOrPieceInStack();

  /** Returns a Stack if unexpanded and overlapping the given point,
   * or a piece within that stack if expanded and overlapping the given point
   */
  PieceFinder MOVABLE = new Movable();

  PieceFinder MAT_ONLY = new MatOnly();

  /**
   * Returns a Mat that overlaps this piece
   */
  class MatOnly extends Movable {
    @Override
    public Object visitDefault(GamePiece piece) {
      if (!"".equals(piece.getProperty(MAT_NAME))) {
        return super.visitDefault(piece);
      }
      return null;
    }

    @Override
    public Object visitStack(Stack s) {
      return null;
    }
  }

  class StackOnly extends Movable {
    @Override
    public Object visitDefault(GamePiece piece) {
      return null;
    }

    @Override
    public Object visitStack(Stack s) {
      GamePiece selected = (GamePiece) super.visitStack(s);
      if (selected != null
          && selected.getParent() == s) {
        selected = s;
      }
      return selected;
    }
  }


  class DeckOrPieceInStack extends PieceInStack {
    @Override
    public Object visitDeck(Deck d) {
      final Shape s = d.getShape();
      final Point pos = d.getPosition();
      final Point p = new Point(pt.x - pos.x, pt.y - pos.y);
      return (s.contains(p) ? d : null);
    }
  }


  class PieceInStack extends Movable {
    @Override
    public Object visitStack(Stack s) {
      GamePiece selected = (GamePiece) super.visitStack(s);
      if (selected == s
          && !s.isExpanded()) {
        selected = s.topPiece();  //NOTE: topPiece() returns the top VISIBLE piece (not hidden by Invisible trait)
      }
      return selected;
    }
  }

  class Movable implements PieceFinder, DeckVisitor {
    protected Shape[] shapes = new Shape[0];
    protected Map map;
    protected Point pt;
    protected DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(this);

    // This constructor is safe only if using the PieceFinder.select() method
    public Movable() {
      this(null, null);
    }

    public Movable(Map map, Point pt) {
      this.map = map;
      this.pt = pt;
    }

    @Override
    public Object visitDeck(Deck d) {
      return null;
    }

    @Override
    public Object visitDefault(GamePiece piece) {
      GamePiece selected = null;
      final Shape s = piece.getShape();
      final Point pos = piece.getPosition();
      final Point p = new Point(pt.x - pos.x, pt.y - pos.y);
      if (s.contains(p)) {
        selected = piece;
      }
      return selected;
    }

    @Override
    public Object visitStack(Stack s) {
      GamePiece selected = null;
      if (shapes.length < s.getPieceCount()) {
        shapes = new Shape[s.getPieceCount()];
      }
      map.getStackMetrics().getContents(s, null, shapes, null, s.getPosition().x, s.getPosition().y);
      for (final Iterator<GamePiece> i = s.getPiecesInVisibleOrderIterator();
           i.hasNext();) {
        final GamePiece child = i.next();

        // Pieces can be moved by background threads causing the size of
        // the Stack to change after the Iterator is generated.
        // FIXME: This is a workaround. We should fix the threading bug
        // which causes.
        final int index = s.indexOf(child);
        if (index >= 0 && index < shapes.length) {
          if (shapes[index].contains(pt)) {
            selected = s.isExpanded() ? child : s;
            break;
          }
        }
      }
      return selected;
    }

    @Override
    public GamePiece select(Map map, GamePiece piece, Point pt) {
      this.map = map;
      this.pt = pt;
      return (GamePiece) dispatcher.accept(piece);
    }
  }
}

