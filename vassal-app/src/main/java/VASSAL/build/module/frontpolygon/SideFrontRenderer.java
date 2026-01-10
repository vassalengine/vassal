package VASSAL.build.module.frontpolygon;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;

import java.awt.BasicStroke;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;

/**
 * Fills the map with side territories using a Voronoi-style partition:
 * each point in the map is colored for the closest side marker, yielding
 * a pair of colored regions with outlined borders at the current zoom level.
 */
class SideFrontRenderer {
  private static final float LINE_WIDTH = 3f;
  private static final double MIN_MARKER_RADIUS = 8;
  private static final double ISLAND_RADIUS = 35;
  private static final double BOUNDS_PADDING = 250;
  private static final double EPSILON = 1e-6;
  private static final BasicStroke OUTLINE_STROKE = new BasicStroke(
    LINE_WIDTH,
    BasicStroke.CAP_ROUND,
    BasicStroke.JOIN_ROUND
  );

  /**
   * Builds Voronoi cells for each side's markers and paints the resulting territories.
   */
  void render(Graphics g, Map map, EnumMap<FrontPolygonSide, List<Point>> sidePoints) {
    final long start = System.nanoTime();
    final Rectangle2D bounds = computeBounds(map);
    if (bounds == null) {
      return;
    }

    final Graphics2D g2 = (Graphics2D) g.create();
    try {
      final double osScale = g2.getDeviceConfiguration().getDefaultTransform().getScaleX();
      final double scale = map.getZoom() * osScale;
      final AffineTransform transform = AffineTransform.getScaleInstance(scale, scale);
      final Area alliedArea = buildTerritoryArea(sidePoints.get(FrontPolygonSide.ALLIES), sidePoints.get(FrontPolygonSide.GERMANS), bounds);
      final Area germanArea = buildTerritoryArea(sidePoints.get(FrontPolygonSide.GERMANS), sidePoints.get(FrontPolygonSide.ALLIES), bounds);

      drawSideArea(g2, transform, alliedArea, FrontPolygonSide.ALLIES);
      drawSideArea(g2, transform, germanArea, FrontPolygonSide.GERMANS);
    }
    finally {
      g2.dispose();
      final long nanos = System.nanoTime() - start;
      FrontPolygon.addSideRenderNanos(nanos);
      final double ms = nanos / 1_000_000.0;
      GameModule.getGameModule().warn(String.format("FrontPolygon side render took %.2f ms", ms));
    }
  }

  private Rectangle2D computeBounds(Map map) {
    if (map == null || map.mapSize() == null) {
      return null;
    }
    final double width = map.mapSize().getWidth();
    final double height = map.mapSize().getHeight();
    if (width <= 0 || height <= 0) {
      return null;
    }
    // Use full map bounds (plus padding) so territories cover the whole board, even where no units sit.
    return new Rectangle2D.Double(
      -BOUNDS_PADDING,
      -BOUNDS_PADDING,
      width + (BOUNDS_PADDING * 2),
      height + (BOUNDS_PADDING * 2)
    );
  }

  private Area buildTerritoryArea(List<Point> ownPoints, List<Point> opponentPoints, Rectangle2D bounds) {
    if (ownPoints == null || ownPoints.isEmpty()) {
      return new Area();
    }

    if (opponentPoints == null || opponentPoints.isEmpty()) {
      return buildStandaloneArea(ownPoints);
    }

    final List<Point2D.Double> boundingPolygon = rectangleToPolygon(bounds);
    final Area territory = new Area();

    for (final Point own : ownPoints) {
      List<Point2D.Double> cell = new ArrayList<>(boundingPolygon);
      for (final Point opponent : opponentPoints) {
        cell = clipCellToOpponent(cell, own, opponent);
        if (cell.isEmpty()) {
          break;
        }
      }

      if (cell.size() >= 3) {
        territory.add(areaFromPolygon(cell));
      }
      else if (!cell.isEmpty()) {
        territory.add(circleArea(centroid(cell), MIN_MARKER_RADIUS));
      }
    }

    return territory;
  }

  private void drawSideArea(Graphics2D g2, AffineTransform transform, Area area, FrontPolygonSide side) {
    if (area == null || area.isEmpty()) {
      return;
    }

    final Area drawingArea = area.createTransformedArea(transform);
    g2.setColor(side.fillColor);
    g2.fill(drawingArea);
    g2.setColor(side.outlineColor);
    g2.setStroke(OUTLINE_STROKE);
    g2.draw(drawingArea);
  }

  private List<Point2D.Double> rectangleToPolygon(Rectangle2D bounds) {
    final List<Point2D.Double> polygon = new ArrayList<>(4);
    polygon.add(new Point2D.Double(bounds.getMinX(), bounds.getMinY()));
    polygon.add(new Point2D.Double(bounds.getMaxX(), bounds.getMinY()));
    polygon.add(new Point2D.Double(bounds.getMaxX(), bounds.getMaxY()));
    polygon.add(new Point2D.Double(bounds.getMinX(), bounds.getMaxY()));
    return polygon;
  }

  private List<Point2D.Double> clipCellToOpponent(List<Point2D.Double> polygon, Point own, Point opponent) {
    if (polygon.isEmpty()) {
      return polygon;
    }

    final double dx = opponent.getX() - own.getX();
    final double dy = opponent.getY() - own.getY();
    final double c = 0.5 * ((opponent.getX() * opponent.getX()) + (opponent.getY() * opponent.getY())
      - (own.getX() * own.getX()) - (own.getY() * own.getY()));

    final List<Point2D.Double> result = new ArrayList<>();
    Point2D.Double previous = polygon.get(polygon.size() - 1);
    boolean previousInside = isInsideHalfPlane(previous, dx, dy, c);

    for (final Point2D.Double current : polygon) {
      final boolean currentInside = isInsideHalfPlane(current, dx, dy, c);

      if (currentInside != previousInside) {
        final Point2D.Double intersection = findIntersection(previous, current, dx, dy, c);
        if (intersection != null) {
          result.add(intersection);
        }
      }

      if (currentInside) {
        result.add(current);
      }

      previous = current;
      previousInside = currentInside;
    }

    return result;
  }

  private boolean isInsideHalfPlane(Point2D.Double point, double dx, double dy, double c) {
    return (dx * point.getX()) + (dy * point.getY()) <= c + EPSILON;
  }

  private Point2D.Double findIntersection(Point2D.Double start, Point2D.Double end, double dx, double dy, double c) {
    final double sx = start.getX();
    final double sy = start.getY();
    final double ex = end.getX();
    final double ey = end.getY();

    final double vx = ex - sx;
    final double vy = ey - sy;
    final double denominator = (dx * vx) + (dy * vy);

    if (Math.abs(denominator) < EPSILON) {
      return null;
    }

    final double t = (c - (dx * sx) - (dy * sy)) / denominator;
    if (t < -EPSILON || t > 1 + EPSILON) {
      return null;
    }

    return new Point2D.Double(sx + (vx * t), sy + (vy * t));
  }

  private Area areaFromPolygon(List<Point2D.Double> points) {
    final Path2D.Double path = new Path2D.Double();
    final Point2D.Double first = points.get(0);
    path.moveTo(first.getX(), first.getY());
    for (int i = 1; i < points.size(); i++) {
      final Point2D.Double point = points.get(i);
      path.lineTo(point.getX(), point.getY());
    }
    path.closePath();
    return new Area(path);
  }

  private Area circleArea(Point2D center, double radius) {
    final Ellipse2D ellipse = new Ellipse2D.Double(
      center.getX() - radius,
      center.getY() - radius,
      radius * 2,
      radius * 2
    );
    return new Area(ellipse);
  }

  private Area buildStandaloneArea(List<Point> points) {
    final Area area = new Area();
    for (final Point point : points) {
      area.add(circleArea(new Point2D.Double(point.getX(), point.getY()), ISLAND_RADIUS));
    }
    return area;
  }

  private Point2D.Double centroid(List<Point2D.Double> points) {
    double sumX = 0;
    double sumY = 0;
    for (final Point2D.Double point : points) {
      sumX += point.getX();
      sumY += point.getY();
    }
    final double size = points.size();
    return new Point2D.Double(sumX / size, sumY / size);
  }
}
