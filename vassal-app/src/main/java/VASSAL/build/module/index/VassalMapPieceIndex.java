/*
 *
 * Copyright (c) 2023 by The VASSAL Development Team
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
package VASSAL.build.module.index;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.GamePiece;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Maintain a set of Indexes about location related information of pieces on a Vassal Map.
 * 1. Maintain a Quadtree of x,y locations to enable fast direct lookup and ranged selections
 * 2. Maintain cross-references of all values for CurrentZone and LocationName to be able to
 *    quickly find all pieces in a specified Zone, Region or module defined location like a Hex.
 */
public class VassalMapPieceIndex {

  /** Quadtree index allowing fast lookup of pieces by position */
  private VassalMapQuadTree qtree;

  /** A property cros-reference of CurrentZone for fast lookup of pieces in a given zone */
  private final PiecePropertyIndex zoneIndex;

  /** A property cross-reference of LocationName for fast lookup of pieces in a given region or board position such as hex */
  private final PiecePropertyIndex locationIndex;

  /** The Vassal Map these indexes apply to */
  private final Map map;

  public VassalMapPieceIndex(Map map) {
    this.map = map;
    qtree = new VassalMapQuadTree(map);
    zoneIndex = new PiecePropertyIndex(BasicPiece.CURRENT_ZONE);
    locationIndex = new PiecePropertyIndex(BasicPiece.LOCATION_NAME);
  }

  /**
   * A piece has been added to our map.
   * @param piece Piece added
   */
  public void addOrUpdatePiece(GamePiece piece) {
    final Point pos = piece.getPosition();
    if (pos != null) {
      // If this piece does not reside within the bounds of the Qtree,
      // Then copy the existing Qtree to a new larger one that encompasses this piece, plus an extra 100 pixels.
      // The Qtree is initially set to the size of the visible play area, so this should not happen often.
      if (! qtree.getBounds().contains(pos)) {
        final Rectangle bounds = qtree.getBounds();
        final int minX = Math.min(pos.x - 100, bounds.x);
        final int maxX = Math.max(pos.x + 100, bounds.x + bounds.width);
        final int minY = Math.min(pos.y - 100, bounds.y);
        final int maxY = Math.max(pos.y + 100, bounds.y + bounds.height);
        qtree = new VassalMapQuadTree(qtree, minX, minY, maxX, maxY);
      }
      qtree.addOrUpdatePiece(piece);
    }
    zoneIndex.addOrUpdatePiece(piece);
    locationIndex.addOrUpdatePiece(piece);
  }

  /**
   * A piece has been removed from our map
   * @param piece Removed piece
   */
  public void removePiece(GamePiece piece) {
    qtree.removePiece(piece);
    zoneIndex.removePiece(piece);
    locationIndex.removePiece(piece);
  }

  /**
   * Return a list of pieces in range of a given piece
   *
   * @param piece Piece to search around
   * @param range Range in units appropriate to the search point
   * @return List of pieces
   */
  public List<GamePiece> getPieces(GamePiece piece, int range) {
    return getPieces(piece, range, false);
  }

  public List<GamePiece> getPieces(GamePiece piece, int range, boolean forceAsPixels) {
    final List<GamePiece> pieces = getPieces(piece.getPosition(), range, forceAsPixels);
    // Do not return the source piece in the list of in-range pieces
    pieces.remove(piece);
    return pieces;
  }

  /**
   * Return a list of pieces in range of a given point.
   * NOTE: Does not need to be accurate, may include pieces out of range, range will be accurately checked later,
   * but must not exclude pieces that are in range
   *
   * @param point Point to search around
   * @param range Range in units appropriate to the search point
   * @return List of pieces
   */
  public List<GamePiece> getPieces(Point point, int range) {
    return getPieces(point, range, false);
  }

  /**
   * Return a list of pieces in range of a given point.
   * NOTE: Does not need to be accurate, may include pieces out of range, range will be accurately checked later,
   * but must not exclude pieces that are in range
   *
   * @param point         Point to search around
   * @param range         Range in units appropriate to the search point
   * @param forceAsPixels Force the search to be in pixels, even if the target point is on a regular grid
   * @return List of pieces
   */
  public List<GamePiece> getPieces(Point point, int range, boolean forceAsPixels) {

    // Determine roughly how many pixels each Range element represents at the point in question
    final int pixelsPerRangeElement = forceAsPixels ? 1 : map.getMaxPixelsPerRangeUnit(point);

    // Determine the pieces in range (ish). Pieces in the Qtree are stored in straight X,Y co-ords, so need
    // to convert the range (if in grid cells) to a pixel distance.
    // The context piece may not be snapped in the hex, so add 1 to the range to cater for offset
    final List<GamePiece> rawPieces = qtree.getPiecesInRange(point, (range + 1) * pixelsPerRangeElement);

    // getPiecesInRange() does a rough search, it returns pieces that will be just out of range, do
    // an accurate range check on each piece
    final List<GamePiece> pieces = new ArrayList<>();
    for (final GamePiece piece : rawPieces) {

      final Point pos = piece.getPosition();
      final MapGrid grid = map.findBoard(pos) == null ? null : map.findBoard(pos).getGrid();

      final double calculatedRange;
      if (grid == null) {
        calculatedRange = Point.distance(point.x, point.y, pos.x, pos.y);
      }
      else {
        calculatedRange = grid.range(point, pos);
      }

      if ((int) Math.floor(calculatedRange + 0.5) <= range) {
        pieces.add(piece);
      }
    }
    return pieces;
  }

  public Set<GamePiece> getPieces(String propertyName, String propertyValue) {
    if (BasicPiece.CURRENT_ZONE.equals(propertyName)) {
      return zoneIndex.getPieces(propertyValue);
    }
    else if (BasicPiece.LOCATION_NAME.equals(propertyName)) {
      return locationIndex.getPieces(propertyValue);
    }
    else {
      return new HashSet<>();
    }
  }
}

