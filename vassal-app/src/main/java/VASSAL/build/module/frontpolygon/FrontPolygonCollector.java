package VASSAL.build.module.frontpolygon;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.counters.Decorator;
import VASSAL.counters.Deck;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Obscurable;
import VASSAL.counters.PieceAccess;
import VASSAL.counters.SpecifiedSideAccess;
import VASSAL.counters.Stack;

import java.awt.Point;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Collects per-side piece positions from the map.
 */
class FrontPolygonCollector {
  private static final boolean DEBUG_PIECES = Boolean.getBoolean("FrontPolygon.debugPieces");

  EnumMap<FrontPolygonSide, List<Point>> collect(Map map) {
    final EnumMap<FrontPolygonSide, List<Point>> positions = new EnumMap<>(FrontPolygonSide.class);
    final EnumMap<FrontPolygonSide, Set<Long>> seenLocations = new EnumMap<>(FrontPolygonSide.class);
    for (final FrontPolygonSide side : FrontPolygonSide.values()) {
      positions.put(side, new ArrayList<>());
      seenLocations.put(side, new HashSet<>());
    }

    final GamePiece[] pieces = map.getAllPieces();
    for (final GamePiece pieceOnMap : pieces) {
      if (DEBUG_PIECES) {
        logPieceDetails(pieceOnMap);
      }
      collectFromPiece(pieceOnMap, map, positions, seenLocations);
    }

    // Debug logging to help diagnose why nothing was collected.
    if (positions.values().stream().allMatch(List::isEmpty)) {
      final Set<String> encounteredMaskOwners = new HashSet<>();
      for (final GamePiece piece : pieces) {
        final Object maskOwner = piece.getProperty(VASSAL.counters.Obscurable.ID);
        if (maskOwner != null) {
          encounteredMaskOwners.add(String.valueOf(maskOwner));
        }
      }
      final StringBuilder sb = new StringBuilder("FrontPolygon debug: no pieces matched. ");
      sb.append("Configured layers: Allies=").append(FrontPolygonSide.ALLIES.getLayerNames())
        .append(", Axis=").append(FrontPolygonSide.GERMANS.getLayerNames()).append(". ");
      sb.append("Configured mask owners: Allies=").append(FrontPolygonSide.ALLIES.getMaskOwners())
        .append(", Axis=").append(FrontPolygonSide.GERMANS.getMaskOwners()).append(". ");
      if (!encounteredMaskOwners.isEmpty()) {
        sb.append("Encountered mask owners=").append(encounteredMaskOwners).append(". ");
      }
      sb.append("Total pieces inspected=").append(pieces.length);
      VASSAL.build.GameModule.getGameModule().warn(sb.toString());
    }

    return positions;
  }

  private void logPieceDetails(GamePiece piece) {
    if (piece == null) {
      return;
    }
    final StringBuilder sb = new StringBuilder("FrontPolygon inspect: ");
    sb.append("name=").append(piece.getName());
    sb.append(", pos=").append(piece.getPosition());

    final Object maskOwner = piece.getProperty(VASSAL.counters.Obscurable.ID);
    sb.append(", maskOwner=").append(maskOwner);

    final StringBuilder matches = new StringBuilder();
    for (final FrontPolygonSide side : FrontPolygonSide.values()) {
      final boolean layerMatch = hasLayer(piece, side);
      final boolean maskAccessMatch = hasMaskAccess(piece, side);
      matches.append(side.name()).append("{layer=").append(layerMatch)
        .append(", maskAccess=").append(maskAccessMatch).append("} ");
    }
    sb.append(", matches=").append(matches);

    GameModule.getGameModule().warn(sb.toString());
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
        logMatch(piece, side, "layer");
        return side;
      }
      if (hasMaskAccess(piece, side)) {
        logMatch(piece, side, "maskAccess");
        return side;
      }
    }
    return null;
  }

  private void logMatch(GamePiece piece, FrontPolygonSide side, String reason) {
    if (!DEBUG_PIECES) {
      return;
    }
    GameModule.getGameModule().warn("FrontPolygon matched piece '" + piece.getName()
      + "' at " + piece.getPosition() + " to side " + side.name() + " via " + reason);
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

  private boolean hasMaskAccess(GamePiece piece, FrontPolygonSide side) {
    if (side.getMaskOwners().isEmpty()) {
      return false;
    }
    final List<GamePiece> obscurables = Decorator.getDecorators(piece, Obscurable.class);
    if (obscurables == null || obscurables.isEmpty()) {
      return false;
    }
    for (final GamePiece deco : obscurables) {
      final PieceAccess access = extractAccess((Obscurable) deco);
      if (access instanceof SpecifiedSideAccess) {
        for (final String allowed : ((SpecifiedSideAccess) access).getSides()) {
          if (side.matchesMaskOwner(allowed)) {
            return true;
          }
        }
      }
    }
    return false;
  }

  private PieceAccess extractAccess(Obscurable obscurable) {
    try {
      final Field f = Obscurable.class.getDeclaredField("access");
      f.setAccessible(true);
      final Object value = f.get(obscurable);
      if (value instanceof PieceAccess) {
        return (PieceAccess) value;
      }
    }
    catch (Exception ignored) {
      // best-effort; return null
    }
    return null;
  }

  private long positionKey(Point point) {
    return (((long) point.x) << 32) ^ (point.y & 0xffffffffL);
  }
}
