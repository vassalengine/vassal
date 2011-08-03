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
package VASSAL.counters;

import java.awt.Point;
import java.awt.Shape;
import java.util.Iterator;

import VASSAL.build.module.Map;

/**
 * This interface defines selection criteria for finding a GamePiece in a Map
 */
public interface PieceFinder {
  /** Return the argument GamePiece (or one of its children if a Stack) found at the given point on the given Map */
  public GamePiece select(Map map, GamePiece piece, Point pt);

  /** Return a Stack overlapping the given point */
  public static final PieceFinder STACK_ONLY = new StackOnly();

  /**
   * If a Stack overlaps the given point, return the piece containing that point if expanded,
   * or the top piece if not expanded.
   * */
  public static final PieceFinder PIECE_IN_STACK = new PieceInStack();

  /** Returns a Stack if unexpanded and overlapping the given point,
   * or a piece within that stack if expanded and overlapping the given point
   */
  public static final PieceFinder MOVABLE = new Movable();

  public static class StackOnly extends Movable {
    public Object visitDefault(GamePiece piece) {
      return null;
    }

    public Object visitStack(Stack s) {
      GamePiece selected = (GamePiece) super.visitStack(s);
      if (selected != null
          && selected.getParent() == s) {
        selected = s;
      }
      return selected;
    }

  }

  public static class PieceInStack extends Movable {
    public Object visitStack(Stack s) {
      GamePiece selected = (GamePiece) super.visitStack(s);
      if (selected == s
          && !s.isExpanded()) {
        selected = s.topPiece();
      }
      return selected;
    }
  }

  public static class Movable implements PieceFinder, DeckVisitor {
    protected Shape[] shapes = new Shape[0];
    protected Map map;
    protected Point pt;
    protected DeckVisitorDispatcher dispatcher = new DeckVisitorDispatcher(this);

    // This constructor is safe only if using the PieceFinder.select() method
    public Movable() {
      this(null,null);
    }

    public Movable(Map map, Point pt) {
      this.map = map;
      this.pt = pt;
    }

    public Object visitDeck(Deck d) {
      return null;
    }

    public Object visitDefault(GamePiece piece) {
      GamePiece selected = null;
      Shape s = piece.getShape();
      Point pos = piece.getPosition();
      Point p = new Point(pt.x - pos.x, pt.y - pos.y);
      if (s.contains(p)) {
        selected = piece;
      }
      return selected;
    }

    public Object visitStack(Stack s) {
      GamePiece selected = null;
      if (shapes.length < s.getPieceCount()) {
        shapes = new Shape[s.getPieceCount()];
      }
      map.getStackMetrics().getContents(s, null, shapes, null, s.getPosition().x, s.getPosition().y);
      for (Iterator<GamePiece> i = s.getPiecesInVisibleOrderIterator();
           i.hasNext();) {
        GamePiece child = i.next();

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

    public GamePiece select(Map map, GamePiece piece, Point pt) {
      this.map = map;
      this.pt = pt;
      return (GamePiece) dispatcher.accept(piece);
    }

  }
}

