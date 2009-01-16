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
package VASSAL.build.module.map;

import VASSAL.counters.GamePiece;
import VASSAL.tools.ArrayUtils;

/** Stores {@link VASSAL.counters.GamePiece}s in a simple array */
public class SimplePieceCollection implements PieceCollection {
  private int capacity = 100, incr = 25;
  private GamePiece pieces[] = new GamePiece[capacity];
  private int pieceCount = 0;

  /**
   * Returns the index of a piece.  When painting the map, pieces
   * are drawn in order of index */
  public int indexOf(GamePiece p) {
    for (int i = 0; i < pieceCount; ++i)
      if (pieces[i] == p)
        return (i);
    return -1;
  }

  public boolean canMerge(GamePiece p1, GamePiece p2) {
    return true;
  }

  public void add(GamePiece p) {
    if (pieceCount >= capacity) {
      capacity += incr;
      GamePiece oldStack[] = pieces;
      pieces = new GamePiece[capacity];
      System.arraycopy(oldStack, 0, pieces, 0, pieceCount);
    }
    pieces[pieceCount++] = p;
  }

  public void clear() {
    pieceCount = 0;
  }

  public void remove(GamePiece p) {
    removePieceAt(indexOf(p));
  }

  public GamePiece[] getPieces() {
    return ArrayUtils.copyOf(pieces, pieceCount);
  }
  
  public GamePiece[] getAllPieces() {
    return getPieces();
  }

  private void removePieceAt(int gone) {
    if (gone >= 0) {
      for (int i = gone; i < pieceCount - 1; ++i)
        pieces[i] = pieces[i + 1];
      pieceCount--;
    }
  }

  public void reposition(GamePiece s, int pos) {
    int index = indexOf(s);
    if (index >= 0) {
      for (int i = index; i < pieceCount - 1; ++i) {
        pieces[i] = pieces[i + 1];
      }
      for (int i = pieceCount - 1; i > pos; --i) {
        pieces[i] = pieces[i - 1];
      }
      pieces[pos] = s;
    }
  }

  public void moveToBack(GamePiece p) {
    reposition(p, 0);
  }

  public void moveToFront(GamePiece p) {
    reposition(p, pieceCount - 1);
  }
}
