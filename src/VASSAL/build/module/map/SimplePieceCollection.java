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

import java.util.ArrayList;

import VASSAL.counters.GamePiece;

/** Stores {@link VASSAL.counters.GamePiece}s in a simple array */
public class SimplePieceCollection implements PieceCollection {
  private final ArrayList<GamePiece> pieces = new ArrayList<GamePiece>();

  /**
   * Returns the index of a piece.  When painting the map, pieces
   * are drawn in order of index */
  public int indexOf(GamePiece p) {
    return pieces.indexOf(p);
  }

  public boolean canMerge(GamePiece p1, GamePiece p2) {
    return true;
  }

  public void add(GamePiece p) {
    pieces.add(p);
  }

  public void clear() {
    pieces.clear();
  }

  public void remove(GamePiece p) {
    removePieceAt(indexOf(p));
  }

  public GamePiece[] getPieces() {
    return pieces.toArray(new GamePiece[pieces.size()]);
  }

  public GamePiece[] getAllPieces() {
    return getPieces();
  }

  private void removePieceAt(int gone) {
    if (gone >= 0) {
      pieces.remove(gone);
    }
  }

  public void reposition(GamePiece p, int pos) {
    final int i = pieces.indexOf(p);
    if (i >= 0) {
      pieces.remove(i);
      pieces.add(pos, p);
    }
  }

  public void moveToBack(GamePiece p) {
    reposition(p, 0);
  }

  public void moveToFront(GamePiece p) {
    final int i = pieces.indexOf(p);
    if (i >= 0) {
      pieces.remove(p);
      pieces.add(p);
    }
  }
}
