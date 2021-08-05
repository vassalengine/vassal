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

/**
 * Defines the properties expected to be defined in each GamePiece
 */
public interface Properties {

  /**
   * Return Boolean.TRUE if the piece cannot be seen by the user
   */
  String INVISIBLE_TO_ME = "Invisible"; // NON-NLS

  String INVISIBLE_TO_OTHERS = "InvisibleToOthers"; // NON-NLS

  String INVISIBLE_DISABLE_AUTO_REPORT_MOVE = "InvisibleDisableAutoReportMoves"; // NON-NLS

  /**
   * Return Boolean.TRUE if the piece's identity is not known to other players
   */
  String OBSCURED_TO_OTHERS = "ObscuredToOthers"; // NON-NLS

  /**
   * Return Boolean.TRUE if the piece's identity is not known to the user
   */
  String OBSCURED_TO_ME = "Obscured"; // NON-NLS

  /**
   * If a piece is obscured to other players, return a String identifying the player who obscured it
   *
   * @see VASSAL.build.GameModule#getUserId
   */
  String OBSCURED_BY = Obscurable.ID;

  /**
   * In order to maintain Legacy support after the fix of bug 12951, we need to save the Obscured state prior to cards
   * being drawn, let it be changed in the draw process, then restore it afterwards. Don't ask.
   */
  String OBSCURED_BY_PRE_DRAW = "ObscuredPreDraw"; // NON-NLS

  /**
   * If non-null, then return an instance of {@link EventFilter}. The piece will not respond to band-select events if the
   * filter rejects them
   */
  String BAND_SELECT_EVENT_FILTER = "bandSelectEventFilter"; // NON-NLS

  /**
   * If non-null, then return an instance of {@link EventFilter}. The piece will not respond to selection events if the
   * filter rejects them
   */
  String SELECT_EVENT_FILTER = "selectEventFilter"; // NON-NLS

  /**
   * If non-null, then return an instance of {@link EventFilter}. The piece will not respond to move events if the
   * filter rejects them
   */
  String MOVE_EVENT_FILTER = "moveEventFilter"; // NON-NLS

  /**
   * If a piece is hidden to other players, return a STring identifying the player who hit it
   *
   * @see VASSAL.build.GameModule#getUserId
   */
  String HIDDEN_BY = Hideable.HIDDEN_BY;

  /**
   * Return Boolean.TRUE if the piece behaves more like a terrain feature than a playing piece
   */
  String TERRAIN = "Immobile"; // NON-NLS

  /**
   * Return Boolean.TRUE if the piece should ignore map grids when being moved
   */
  String IGNORE_GRID = "IgnoreGrid"; // NON-NLS

  /** Return Boolean.TRUE if the piece does not form stacks */
  String NO_STACK = "NoStack"; // NON-NLS

  /**
   * Return Boolean.TRUE if the piece has been marked as selected
   */
  String SELECTED = "Selected"; // NON-NLS

  /**
   * Return a KeyCommand[] object representing the popup menu equivalencies for the key commands recognized by this
   * piece
   */
  String KEY_COMMANDS = "KeyCommands"; // NON-NLS

  /**
   * If this piece is a Decorator, return the decorated piece
   */
  String INNER = "Inner"; // NON-NLS

  /**
   * If this piece decorated by a Decorator, return the Decorator
   */
  String OUTER = "Outer"; // NON-NLS

  /**
   * Return Boolean.TRUE if this piece has Restricted Access
   */
  String RESTRICTED = "Restricted"; // NON-NLS

  /**
   * Return Boolean.TRUE if this piece has movement restricted by a Restricted Access trait   */
  String RESTRICTED_MOVEMENT = "RestrictedMovement"; // NON-NLS


  /** Return Boolean.TRUE if this piece has been moved */
  String MOVED = "Moved"; // NON-NLS

  /** Used to store a duplicate of the target piece at some point in time */
  String SNAPSHOT = "snapshot"; // NON-NLS

  /**
   * If Boolean.TRUE, then treat the piece as if it were not rotated. This effects the value returned by
   * {@link GamePiece#getShape} and the way the piece is drawn.
   *
   * @see FreeRotator
   */
  String USE_UNROTATED_SHAPE = "useUnrotatedShape"; // NON-NLS

  /**
   * Return a String representing the visible features of the piece. If this String changes value, then the piece should
   * be refreshed
   */
  String VISIBLE_STATE = "visibleState"; // NON-NLS

  /** Return Boolean.TRUE if the piece can never be moved */
  String NON_MOVABLE = "cannotMove"; // NON-NLS

  /** Global Piece Id */
  String PIECE_ID = "PieceId"; // NON-NLS
}
