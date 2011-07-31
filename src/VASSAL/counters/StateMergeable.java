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
 * {@link GamePiece}s may implement this interface to implement "merge" functionality of
 * their state.  The <code>merge</code> method applies the diff between an "old" and a "new"
 * state to the current state of the piece.
 */
public interface StateMergeable {
  /**
   * Compute the difference between <code>newState</code> and <code>oldState</code>
   * and appy that difference to the current state
   * @param newState
   * @param oldState
   */
  void mergeState(String newState, String oldState);
}
