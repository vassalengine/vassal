/*
 *
 * Copyright (c) 2000-2024.
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
package VASSAL.build.module.map;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.command.Command;
import VASSAL.counters.Decorator;
import VASSAL.counters.Deck;
import VASSAL.counters.Embellishment;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;

import javax.swing.JButton;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.HashSet;

/**
 * Simple runtime-only polygon overlay that can be toggled via a toolbar button or console command.
 */
public class FrontPolygon implements GameComponent, Drawable {
  private static final float LINE_WIDTH = 3f;
  private static final double MIN_MARKER_RADIUS = 8;
  private static final double ISLAND_RADIUS = 35;
  private static final double BOUNDS_PADDING = 250;
  private static final double EPSILON = 1e-6;
  private static final String LAYER_PREFIX = "Layer - ";

  private static final Set<FrontPolygon> INSTANCES = ConcurrentHashMap.newKeySet();

  private enum Side {
    ALLIES(
      new Color(39, 110, 241, 70),
      new Color(39, 110, 241),
      List.of("Show All US")
    ),
    GERMANS(
      new Color(214, 68, 68, 70),
      new Color(214, 68, 68),
      List.of("Show All Ger", "Show All German")
    );

    private final Color fillColor;
    private final Color outlineColor;
    private final List<String> layerNames;

    Side(Color fillColor, Color outlineColor, List<String> layerNames) {
      this.fillColor = fillColor;
      this.outlineColor = outlineColor;
      this.layerNames = layerNames;
    }

    boolean matchesLayer(String description) {
      if (description == null) {
        return false;
      }
      final String normalized = normalizeLayerDescription(description);
      for (final String candidate : layerNames) {
        if (candidate.equalsIgnoreCase(normalized)) {
          return true;
        }
      }
      return false;
    }
  }

  private static final BasicStroke OUTLINE_STROKE = new BasicStroke(
    LINE_WIDTH,
    BasicStroke.CAP_ROUND,
    BasicStroke.JOIN_ROUND
  );

  private final Map map;
  private final JButton toggleButton;

  private boolean visible;

  public FrontPolygon(Map map) {
    this.map = map;

    toggleButton = new JButton("Front Line");
    toggleButton.setFocusable(false);
    toggleButton.addActionListener(e -> toggle());

    map.getToolBar().add(toggleButton);
    map.getToolBar().revalidate();
    map.addDrawComponent(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    INSTANCES.add(this);
    updateButtonText();
  }

  private void toggle() {
    setVisible(!visible);
  }

  private void setVisible(boolean show) {
    if (visible == show) {
      return;
    }
    visible = show;
    updateButtonText();
    map.repaint();
  }

  private void updateButtonText() {
    toggleButton.setText(visible ? "Hide Front Line" : "Show Front Line");
  }

  public void dispose() {
    INSTANCES.remove(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    map.removeDrawComponent(this);
    map.getToolBar().remove(toggleButton);
    map.getToolBar().revalidate();
    map.getToolBar().repaint();
  }

  public static Optional<Boolean> toggleAllInstances() {
    if (INSTANCES.isEmpty()) {
      return Optional.empty();
    }

    final boolean anyVisible = INSTANCES.stream().anyMatch(FrontPolygon::isVisible);
    final boolean newState = !anyVisible;
    INSTANCES.forEach(instance -> instance.setVisible(newState));
    return Optional.of(newState);
  }

  private boolean isVisible() {
    return visible;
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (!visible) {
      return;
    }

    final EnumMap<Side, List<Point>> sidePoints = collectSidePoints();
    if (sidePoints.values().stream().allMatch(List::isEmpty)) {
      return;
    }

    final Rectangle2D bounds = computeBounds(sidePoints);
    if (bounds == null) {
      return;
    }

    final Graphics2D g2 = (Graphics2D) g.create();
    try {
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

      final double osScale = g2.getDeviceConfiguration().getDefaultTransform().getScaleX();
      final double scale = this.map.getZoom() * osScale;
      final AffineTransform transform = AffineTransform.getScaleInstance(scale, scale);
      final Area alliedArea = buildTerritoryArea(sidePoints.get(Side.ALLIES), sidePoints.get(Side.GERMANS), bounds);
      final Area germanArea = buildTerritoryArea(sidePoints.get(Side.GERMANS), sidePoints.get(Side.ALLIES), bounds);

      drawSideArea(g2, transform, alliedArea, Side.ALLIES);
      drawSideArea(g2, transform, germanArea, Side.GERMANS);
    }
    finally {
      g2.dispose();
    }
  }

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  @Override
  public void setup(boolean gameStarting) {
    // Reset to hidden whenever a game starts/stops.
    setVisible(false);
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  private EnumMap<Side, List<Point>> collectSidePoints() {
    final EnumMap<Side, List<Point>> positions = new EnumMap<>(Side.class);
    final EnumMap<Side, Set<Long>> seenLocations = new EnumMap<>(Side.class);
    for (final Side side : Side.values()) {
      positions.put(side, new ArrayList<>());
      seenLocations.put(side, new HashSet<>());
    }

    for (final GamePiece pieceOnMap : map.getAllPieces()) {
      collectFromPiece(pieceOnMap, positions, seenLocations);
    }

    return positions;
  }

  private Rectangle2D computeBounds(EnumMap<Side, List<Point>> positions) {
    double minX = Double.POSITIVE_INFINITY;
    double minY = Double.POSITIVE_INFINITY;
    double maxX = Double.NEGATIVE_INFINITY;
    double maxY = Double.NEGATIVE_INFINITY;
    boolean hasPoints = false;

    for (final List<Point> points : positions.values()) {
      for (final Point point : points) {
        hasPoints = true;
        minX = Math.min(minX, point.getX());
        minY = Math.min(minY, point.getY());
        maxX = Math.max(maxX, point.getX());
        maxY = Math.max(maxY, point.getY());
      }
    }

    if (!hasPoints) {
      return null;
    }

    final double width = Math.max(10, maxX - minX);
    final double height = Math.max(10, maxY - minY);

    return new Rectangle2D.Double(
      minX - BOUNDS_PADDING,
      minY - BOUNDS_PADDING,
      width + (BOUNDS_PADDING * 2),
      height + (BOUNDS_PADDING * 2)
    );
  }

  private void collectFromPiece(GamePiece piece, java.util.Map<Side, List<Point>> positions, java.util.Map<Side, Set<Long>> seenLocations) {
    if (piece == null || piece.getMap() != map) {
      return;
    }

    if (piece instanceof Deck) {
      return;
    }

    if (piece instanceof Stack) {
      for (final GamePiece inner : ((Stack) piece).asList()) {
        collectFromPiece(inner, positions, seenLocations);
      }
      return;
    }

    final Side side = determineSide(piece);
    if (side == null) {
      return;
    }

    final Point location = new Point(piece.getPosition());
    final long key = positionKey(location);
    if (seenLocations.get(side).add(key)) {
      positions.get(side).add(location);
    }
  }

  private Side determineSide(GamePiece piece) {
    for (final Side side : Side.values()) {
      if (hasLayer(piece, side)) {
        return side;
      }
    }
    return null;
  }

  private boolean hasLayer(GamePiece piece, Side side) {
    final List<GamePiece> decorations = Decorator.getDecorators(piece, Embellishment.class);
    for (final GamePiece decoration : decorations) {
      final Embellishment layer = (Embellishment) decoration;
      if (side.matchesLayer(layer.getDescription())) {
        return true;
      }
    }
    return false;
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

  private void drawSideArea(Graphics2D g2, AffineTransform transform, Area area, Side side) {
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

  private static String normalizeLayerDescription(String description) {
    final String trimmed = description.trim();
    if (trimmed.regionMatches(true, 0, LAYER_PREFIX, 0, LAYER_PREFIX.length())) {
      return trimmed.substring(LAYER_PREFIX.length()).trim();
    }
    return trimmed;
  }

  private long positionKey(Point point) {
    return (((long) point.x) << 32) ^ (point.y & 0xffffffffL);
  }
}
