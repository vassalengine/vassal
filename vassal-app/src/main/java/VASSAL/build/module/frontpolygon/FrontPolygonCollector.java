package VASSAL.build.module.frontpolygon;

import VASSAL.build.module.Map;
import VASSAL.counters.Decorator;
import VASSAL.counters.Deck;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;

import java.awt.Point;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Collects per-side piece positions from the map.
 */
class FrontPolygonCollector {

  EnumMap<FrontPolygonSide, List<Point>> collect(Map map) {
    final EnumMap<FrontPolygonSide, List<Point>> positions = new EnumMap<>(FrontPolygonSide.class);
    final EnumMap<FrontPolygonSide, Set<Long>> seenLocations = new EnumMap<>(FrontPolygonSide.class);
    for (final FrontPolygonSide side : FrontPolygonSide.values()) {
      positions.put(side, new ArrayList<>());
      seenLocations.put(side, new HashSet<>());
    }

    for (final GamePiece pieceOnMap : map.getAllPieces()) {
      collectFromPiece(pieceOnMap, map, positions, seenLocations);
    }

    return positions;
  }

  private void collectFromPiece(GamePiece piece,
                                Map map,
                                java.util.Map<FrontPolygonSide, List<Point>> positions,
                                java.util.Map<FrontPolygonSide, Set<Long>> seenLocations) {
    if (piece == null || piece.getMap() != map) {
      return;
    }

    if (piece instanceof Deck) {
      return;
    }

    if (piece instanceof Stack) {
      for (final GamePiece inner : ((Stack) piece).asList()) {
        collectFromPiece(inner, map, positions, seenLocations);
      }
      return;
    }

    final FrontPolygonSide side = determineSide(piece);
    if (side == null) {
      return;
    }

    final Point location = new Point(piece.getPosition());
    final long key = positionKey(location);
    if (seenLocations.get(side).add(key)) {
      positions.get(side).add(location);
    }
  }

  private FrontPolygonSide determineSide(GamePiece piece) {
    for (final FrontPolygonSide side : FrontPolygonSide.values()) {
      if (hasLayer(piece, side)) {
        return side;
      }
    }
    return null;
  }

  private boolean hasLayer(GamePiece piece, FrontPolygonSide side) {
    final List<GamePiece> decorations = Decorator.getDecorators(piece, Embellishment.class);
    for (final GamePiece decoration : decorations) {
      final Embellishment layer = (Embellishment) decoration;
      if (side.matchesLayer(layer.getDescription())) {
        return true;
      }
    }
    return false;
  }

  private long positionKey(Point point) {
    return (((long) point.x) << 32) ^ (point.y & 0xffffffffL);
  }
}
