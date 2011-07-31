package VASSAL.counters;

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

/**
 * For implementing a somewhat modified Visitor pattern for {@link GamePiece}s.
 * @see PieceVisitorDispatcher
 */
public interface PieceVisitor {
  /** Perform the operation on a Stack */
  public Object visitStack(Stack s);
  /** GamePieces that are not handled by one of the type-specific methods (e.g. {@link #visitStack}) are handled here */
  public Object visitDefault(GamePiece p);
}
