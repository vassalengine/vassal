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

/**
 * Defines the properties expected to be defined in each GamePiece
 */
public interface Properties {

  /**
   * Return Boolean.TRUE if the piece cannot be seen by the user
   */
  public static final String INVISIBLE_TO_ME = "Invisible";

  public static final String INVISIBLE_TO_OTHERS = "InvisibleToOthers";

  /**
   * Return Boolean.TRUE if the piece's identity is not known to other players
   */
  public static final String OBSCURED_TO_OTHERS = "ObscuredToOthers";

  /**
   * Return Boolean.TRUE if the piece's identity is not known to the user
   */
  public static final String OBSCURED_TO_ME = "Obscured";

  /**
   * If a piece is obscured to other players, return a String identifying the player who obscured it
   *
   * @see VASSAL.build.GameModule#getUserId
   */
  public static final String OBSCURED_BY = Obscurable.ID;

  /**
   * If non-null, then return an instance of {@link EventFilter}. The piece will not respond to selection events if the
   * filter rejects them
   */
  public static final String SELECT_EVENT_FILTER = "selectEventFilter";

  /**
   * If non-null, then return an instance of {@link EventFilter}. The piece will not respond to move events if the
   * filter rejects them
   */
  public static final String MOVE_EVENT_FILTER = "moveEventFilter";

  /**
   * If a piece is hidden to other players, return a STring identifying the player who hit it
   *
   * @see VASSAL.build.GameModule#getUserId
   */
  public static final String HIDDEN_BY = Hideable.HIDDEN_BY;

  /**
   * Return Boolean.TRUE if the piece behaves more like a terrain feature than a playing piece
   */
  public static final String TERRAIN = "Immobile";

  /**
   * Return Boolean.TRUE if the piece should ignore map grids when being moved
   */
  public static final String IGNORE_GRID = "IgnoreGrid";

  /** Return Boolean.TRUE if the piece does not form stacks */
  public static final String NO_STACK = "NoStack";

  /**
   * Return Boolean.TRUE if the piece has been marked as selected
   */
  public static final String SELECTED = "Selected";

  /**
   * Return a KeyCommand[] object representing the popup menu equivalencies for the key commands recognized by this
   * piece
   */
  public static final String KEY_COMMANDS = "KeyCommands";

  /**
   * If this piece is a Decorator, return the decorated piece
   */
  public static final String INNER = "Inner";

  /**
   * If this piece decorated by a Decorator, return the Decorator
   */
  public static final String OUTER = "Outer";

  /**
   * Return Boolean.TRUE if this piece has Restricted Access
   */
  public static final String RESTRICTED = "Restricted";

  /**
   * Return Boolean.TRUE if this piece has movement restricted by a Restricted Access trait   */
  public static final String RESTRICTED_MOVEMENT = "RestrictedMovement";


  /** Return Boolean.TRUE if this piece has been moved */
  public static final String MOVED = "Moved";

  /** Used to store a duplicate of the target piece at some point in time */
  public static final String SNAPSHOT = "snapshot";

  /**
   * If Boolean.TRUE, then treat the piece as if it were not rotated. This effects the value returned by
   * {@link GamePiece#getShape} and the way the piece is drawn.
   *
   * @see FreeRotator
   */
  public static final String USE_UNROTATED_SHAPE = "useUnrotatedShape";

  /**
   * Return a String representing the visible features of the piece. If this String changes value, then the piece should
   * be refreshed
   */
  public static final String VISIBLE_STATE = "visibleState";

  /** Return Boolean.TRUE if the piece can never be moved */
  public static final String NON_MOVABLE = "cannotMove";

  /** Global Piece Id */
  public static final String PIECE_ID = "PieceId";
}
