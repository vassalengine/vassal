/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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
 * Defines ability of a player to access a GamePiece
 * @author rkinney
 *
 */
public interface PieceAccess {
  /**
   * Return a String identifying the current player
   */
  String getCurrentPlayerId();
  /**
   * Return true if the current player can access this piece,
   * given that the piece is owned by the player with the given id
   */
  boolean currentPlayerHasAccess(String ownerId);

  /**
   * Return true if the current player can take (or relinquish)
   * control of this piece
   */
  boolean currentPlayerCanModify(String ownerId);

  /**
   * Global utility class to temporarily restrict access to all piece
   * @author rkinney
   *
   */
  public static class GlobalAccess {
    private static boolean allHidden=false;
    public static void hideAll() {
      allHidden = true;
    }
    public static void revertAll() {
      allHidden = false;
    }
    public static boolean isHideAll() {
      return allHidden;
    }
  }
}
