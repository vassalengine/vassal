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

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;

import java.awt.Point;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * The IndexManager maintains a set of indexes on GamePieces that will primarily be used by
 * the GKC Fast-match mechanism to quickly find sets of pieces that meet certain critera.
 *
 * It maintains the following indexes:
 *  o A per-map quadtree of piece locations
 *  o A per-map cross-reference of values of the Currentzone and LocationName properties
 *
 *  The indexes for each Map are stored in a VassalMapPieceIndex object
 *
 *  NOTE: Pieces not on Maps are NOT included in any index.
 */
public class IndexManager {

  /**
   * HashMap of the indexes created so far
   */
  private final java.util.Map<Map, VassalMapPieceIndex> indexes = new HashMap<>();

  /**
   * Return the combined index object for the specified Map, create one if needed
   *
   * @param map Map
   * @return Index
   */
  private VassalMapPieceIndex getIndex(Map map) {
    return indexes.computeIfAbsent(map, m -> new VassalMapPieceIndex(m));
  }

  /**
   * Clear and rebuild all indexes using pieces currently existing on boards.
   * Will usually only be called at the start of a game session after initial load of gamestate is complete.
   */
  public void rebuild() {
    clearAll();
    for (final GamePiece piece : GameModule.getGameModule().getGameState().getAllPieces()) {
      final Map map = piece.getMap();
      if (map != null) {
        pieceMoved(piece, map);
      }
    }
  }

  /**
   * Clear all indexes
   */
  public void clearAll() {
    indexes.clear();
  }

  /**
   * A piece has just moved or been placed on the board, update any location based indexes
   * and create any property based indexes if we have not seen this piece before
   *
   * @param piece Piece that moved
   * @param map   Map that the piece has arrived on
   */
  public void pieceMoved(GamePiece piece, Map map) {
    if (map != null) {
      if (piece instanceof Stack) {
        for (final GamePiece p : ((Stack) piece).asList()) {
          getIndex(map).addOrUpdatePiece(p);
        }
      }
      else {
        getIndex(map).addOrUpdatePiece(piece);
      }
    }
  }

  /**
   * A piece has been removed from a Map
   *
   * @param piece Removed piece
   * @param map   Map piece was removed from
   */
  public void pieceRemoved(GamePiece piece, Map map) {
    if (map != null) {
      if (piece instanceof Stack) {
        for (final GamePiece p : ((Stack) piece).asList()) {
          getIndex(map).removePiece(p);
        }
      }
      else {
        getIndex(map).removePiece(piece);
      }
    }
  }

  /**
   * Return a list of pieces within a given range of another piece
   *
   * @param piece         Piece to use as origin
   * @param range         range in units appropriate to the grid at the pieces location
   * @param forceAsPixels Force the range check to be in pixels, overriding any grid at the target point
   * @return List of pieces (not including the souurce piece)
   */
  public List<GamePiece> getPieces(GamePiece piece, int range, boolean forceAsPixels) {
    final Map map = piece.getMap();
    final VassalMapPieceIndex index = getIndex(map);
    return index.getPieces(piece, range, forceAsPixels);
  }
  public List<GamePiece> getPieces(GamePiece piece, int range) {
    return getPieces(piece, range, false);
  }

  /**
   * Return a list of pieces within a given range of a specified point
   *
   * @param map           Map
   * @param point         Position to search from
   * @param range         Range in units appropriate to the grid at the search point.
   * @param forceAsPixels Force the range check to be in pixels, overriding any grid at the target point
   * @return              List of pieces
   */
  public List<GamePiece> getPieces(Map map, Point point, int range, boolean forceAsPixels) {
    final VassalMapPieceIndex index = getIndex(map);
    return index.getPieces(point, range, forceAsPixels);
  }

  public List<GamePiece> getPieces(Map map, Point point, int range) {
    return getPieces(map, point, range, false);
  }

  /**
   * Return a list of pieces at a specific point on a specific map
   *
   * @param map Map to check
   * @param point Point to check
   * @return Pieces at that Map.Point
   */
  public List<GamePiece> getPieces(Map map, Point point) {
    return getPieces(map, point, 0);
  }

  /**
   * Return a list of pieces on a Map with a specified value for the specified property
   *
   * @param map Map
   * @param propertyName Propertyname to lookup
   * @param propertyValue Property value to look for
   * @return List of pieces with that property value
   */
  public List<GamePiece> getPieces(Map map, String propertyName, String propertyValue) {
    final VassalMapPieceIndex index = getIndex(map);

    // Convert the Set result or null to an ArrayList for call consistency
    return new ArrayList<>(index.getPieces(propertyName, propertyValue));
  }
}
