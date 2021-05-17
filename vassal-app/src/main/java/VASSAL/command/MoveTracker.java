package VASSAL.command;

import java.awt.Point;

import VASSAL.build.GameModule;
import VASSAL.counters.GamePiece;

/*
 *
 * Copyright (c) 2003 by Rodney Kinney
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
 * Convenience class for building {@link MovePiece} commands.
 * @see ChangeTracker
 */
public class MoveTracker {
  private final String oldMapId;
  private final Point oldPosition;
  private final String oldUnderneathId;
  private final GamePiece piece;

  public MoveTracker(GamePiece piece) {
    this.piece = piece;
    oldMapId = getMapId();
    oldPosition = piece.getPosition();
    oldUnderneathId = getUnderneathId();
  }

  private String getUnderneathId() {
    String id = null;
    if (piece.getParent() != null) {
      final int index = piece.getParent().indexOf(piece);
      if (index > 0) {
        id = piece.getParent().getPieceAt(index - 1).getId();
      }
    }
    return id;
  }

  private String getMapId() {
    return piece.getMap() == null ? null : piece.getMap().getIdentifier();
  }

  public Command getMoveCommand() {
    return new MovePiece(
      piece.getId(),
      getMapId(),
      piece.getPosition(),
      getUnderneathId(),
      oldMapId,
      oldPosition,
      oldUnderneathId,
      GameModule.getActiveUserId());
  }
}
