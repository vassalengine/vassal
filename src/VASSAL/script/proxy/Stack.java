/*
 * $Id$
 *
 * Copyright (c) 2008 by Brent Easton
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
package VASSAL.script.proxy;

import java.util.ArrayList;
import java.util.List;


/**
 * BeanShell proxy for the Vassal GamePiece object
 *
 * BeanShell proxies control access to Vassal components and expose
 * a consistent API to BeanShell Scripts.
 *
 * PLEASE NOTE
 * New methods may be added, but DO NOT remove or change the
 * call signature of any existing methods
 *
 */

public class Stack extends GamePiece {

  public Stack(VASSAL.counters.Stack s) {
    super(s);
  }

  public Stack getParent() {
    return null;
  }

  private VASSAL.counters.Stack getVassalStack() {
    return (VASSAL.counters.Stack) getVassalGamePiece();
  }

  public GamePiece topPiece() {
    return new GamePiece(getVassalStack().topPiece());
  }

  public GamePiece bottomPiece() {
    return new GamePiece(getVassalStack().bottomPiece());
  }

  public int getPieceCount() {
    return getVassalStack().getPieceCount();
  }

  public GamePiece getPieceAt(int i) {
    return new GamePiece(getVassalStack().getPieceAt(i));
  }

  public List<GamePiece> getPieces() {
    ArrayList<GamePiece> list = new ArrayList<GamePiece>(getPieceCount());
    for (int i = 0; i < getPieceCount(); i++) {
      list.add(new GamePiece(getVassalStack().getPieceAt(i)));
    }
    return list;
  }
}