package VASSAL.build.module.frontpolygon;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.EnumMap;
import java.util.List;

/**
 * Paints per-hex ownership by finding the nearest side marker to each hex center
 * on every hex grid board, then filling the hex area in that side's color
 * (respecting board transforms, magnification, sideways grids, and clip bounds).
 */
class HexFrontRenderer {

  /**
   * Colors every visible hex cell according to its nearest side marker.
   */
  void render(Graphics g, Map map, EnumMap<FrontPolygonSide, List<Point>> sidePoints) {
    final long start = System.nanoTime();
    final Graphics2D g2 = (Graphics2D) g.create();
    try {
      final Rectangle clipBounds = g2.getClipBounds();
      final double osScale = g2.getDeviceConfiguration().getDefaultTransform().getScaleX();
      final Rectangle mapClip = clipBounds == null ? null : map.drawingToMap(new Rectangle(clipBounds), osScale);
      final double drawingScale = map.getZoom() * osScale;

      for (final Board board : map.getBoards()) {
        final HexGrid hexGrid = findHexGrid(board.getGrid());
        if (hexGrid == null) {
          continue;
        }
        colorizeBoardHexes(g2, board, hexGrid, sidePoints, mapClip, drawingScale);
      }
    }
    finally {
      g2.dispose();
      final long nanos = System.nanoTime() - start;
      FrontPolygon.addHexRenderNanos(nanos);
      final double ms = nanos / 1_000_000.0;
      GameModule.getGameModule().warn(String.format("FrontPolygon hex render took %.2f ms", ms));
    }
  }

  private HexGrid findHexGrid(MapGrid grid) {
    if (grid instanceof HexGrid) {
      return (HexGrid) grid;
    }
    if (grid instanceof ZonedGrid) {
      final ZonedGrid zonedGrid = (ZonedGrid) grid;
      final MapGrid background = zonedGrid.getBackgroundGrid();
      if (background instanceof HexGrid) {
        return (HexGrid) background;
      }
      for (final Zone zone : zonedGrid.getZonesList()) {
        final HexGrid zoneGrid = findHexGrid(zone.getGrid());
        if (zoneGrid != null) {
          return zoneGrid;
        }
      }
    }
    return null;
  }

  private void colorizeBoardHexes(Graphics2D g2,
                                  Board board,
                                  HexGrid grid,
                                  EnumMap<FrontPolygonSide, List<Point>> sidePoints,
                                  Rectangle mapClip,
                                  double drawingScale) {
    final Rectangle boardBounds = board.bounds();
    if (mapClip != null && !boardBounds.intersects(mapClip)) {
      return;
    }

    final double magnification = board.getMagnification();
    if (magnification == 0) {
      return;
    }

    double localWidth = boardBounds.width / magnification;
    double localHeight = boardBounds.height / magnification;
    if (grid.isSideways()) {
      final double temp = localWidth;
      localWidth = localHeight;
      localHeight = temp;
    }

    final double dx = grid.getDx();
    final double dy = grid.getDy();
    if (dx <= 0 || dy <= 0) {
      return;
    }

    final double startX = alignToGrid(grid.getOrigin().x, 2 * dx);
    final double startY = alignToGrid(grid.getOrigin().y, dy);
    final double endX = localWidth + (2 * dx);
    final double endY = localHeight + dy;

    final AffineTransform boardTransform = buildBoardTransform(board, boardBounds);
    final Area boardArea = new Area(new Rectangle2D.Double(boardBounds.x, boardBounds.y, boardBounds.width, boardBounds.height));

    for (double x = startX; x < endX; x += 2 * dx) {
      for (double y = startY; y < endY; y += dy) {
        processHexCenter(g2, grid, sidePoints, boardTransform, boardArea, mapClip, x, y, drawingScale);
        processHexCenter(g2, grid, sidePoints, boardTransform, boardArea, mapClip, x + dx, y + dy / 2, drawingScale);
      }
    }
  }

  private double alignToGrid(double origin, double step) {
    if (step == 0) {
      return origin;
    }
    return origin - step * Math.ceil(origin / step);
  }

  private void processHexCenter(Graphics2D g2,
                                HexGrid grid,
                                EnumMap<FrontPolygonSide, List<Point>> sidePoints,
                                AffineTransform boardTransform,
                                Area boardArea,
                                Rectangle mapClip,
                                double rawX,
                                double rawY,
                                double drawingScale) {
    final Point center = new Point((int) Math.round(rawX), (int) Math.round(rawY));
    grid.rotateIfSideways(center);

    final Area hexArea = grid.getGridShape(center, 0);
    if (hexArea == null || hexArea.isEmpty()) {
      return;
    }

    final Area mapArea = hexArea.createTransformedArea(boardTransform);
    mapArea.intersect(boardArea);
    if (mapArea.isEmpty()) {
      return;
    }

    if (mapClip != null && !mapArea.getBounds2D().intersects(mapClip)) {
      return;
    }

    final Point2D.Double mapCenter = new Point2D.Double(center.x, center.y);
    boardTransform.transform(mapCenter, mapCenter);
    final FrontPolygonSide owner = findNearestSide(mapCenter, sidePoints);
    if (owner == null) {
      return;
    }

    final AffineTransform drawingTransform = AffineTransform.getScaleInstance(drawingScale, drawingScale);
    final Area drawingArea = mapArea.createTransformedArea(drawingTransform);
    g2.setColor(owner.fillColor);
    g2.fill(drawingArea);
  }

  private AffineTransform buildBoardTransform(Board board, Rectangle boardBounds) {
    final AffineTransform tx = new AffineTransform();
    tx.translate(boardBounds.x, boardBounds.y);
    if (board.isReversed()) {
      tx.translate(boardBounds.width, boardBounds.height);
      tx.scale(-1, -1);
    }
    final double magnification = board.getMagnification();
    if (magnification != 1.0) {
      tx.scale(magnification, magnification);
    }
    return tx;
  }

  private FrontPolygonSide findNearestSide(Point2D point, EnumMap<FrontPolygonSide, List<Point>> sidePoints) {
    FrontPolygonSide closest = null;
    double closestDistance = Double.POSITIVE_INFINITY;
    for (final FrontPolygonSide side : FrontPolygonSide.values()) {
      final List<Point> candidates = sidePoints.get(side);
      if (candidates == null || candidates.isEmpty()) {
        continue;
      }
      final double distance = closestDistanceSquared(point, candidates);
      if (distance < closestDistance) {
        closestDistance = distance;
        closest = side;
      }
    }
    return closest;
  }

  private double closestDistanceSquared(Point2D point, List<Point> candidates) {
    double best = Double.POSITIVE_INFINITY;
    for (final Point candidate : candidates) {
      final double dx = candidate.getX() - point.getX();
      final double dy = candidate.getY() - point.getY();
      final double distance = (dx * dx) + (dy * dy);
      if (distance < best) {
        best = distance;
      }
    }
    return best;
  }
}
