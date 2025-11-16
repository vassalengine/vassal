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
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;

import javax.swing.JButton;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
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
  private final JButton hexButton;
  private final JButton sideButton;

  private boolean hexVisible;
  private boolean sideVisible;

  public FrontPolygon(Map map) {
    this.map = map;

    hexButton = new JButton("Hex Front");
    hexButton.setFocusable(false);
    hexButton.addActionListener(e -> toggleHex());

    sideButton = new JButton("Side Front");
    sideButton.setFocusable(false);
    sideButton.addActionListener(e -> toggleSide());

    map.getToolBar().add(hexButton);
    map.getToolBar().add(sideButton);
    map.getToolBar().revalidate();
    map.addDrawComponent(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    INSTANCES.add(this);
    updateButtonText();
  }

  private void toggleHex() {
    setHexVisible(!hexVisible);
  }

  private void toggleSide() {
    setSideVisible(!sideVisible);
  }

  private void setHexVisible(boolean show) {
    if (hexVisible == show) {
      return;
    }
    hexVisible = show;
    updateButtonText();
    map.repaint();
  }

  private void setSideVisible(boolean show) {
    if (sideVisible == show) {
      return;
    }
    sideVisible = show;
    updateButtonText();
    map.repaint();
  }

  private void updateButtonText() {
    hexButton.setText(hexVisible ? "Hide Hex Front" : "Show Hex Front");
    sideButton.setText(sideVisible ? "Hide Side Front" : "Show Side Front");
  }

  public void dispose() {
    INSTANCES.remove(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    map.removeDrawComponent(this);
    map.getToolBar().remove(hexButton);
    map.getToolBar().remove(sideButton);
    map.getToolBar().revalidate();
    map.getToolBar().repaint();
  }

  public static Optional<Boolean> toggleAllInstances() {
    if (INSTANCES.isEmpty()) {
      return Optional.empty();
    }

    final boolean anyVisible = INSTANCES.stream().anyMatch(FrontPolygon::isAnyVisible);
    final boolean newState = !anyVisible;
    INSTANCES.forEach(instance -> instance.setBothVisible(newState));
    return Optional.of(newState);
  }

  private boolean isAnyVisible() {
    return hexVisible || sideVisible;
  }

  private void setBothVisible(boolean visible) {
    hexVisible = visible;
    sideVisible = visible;
    updateButtonText();
    map.repaint();
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (!hexVisible && !sideVisible) {
      return;
    }

    final Graphics2D g2 = (Graphics2D) g.create();
    try {
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

      final EnumMap<Side, List<Point>> sidePoints = collectSidePoints();
      if (sidePoints.values().stream().allMatch(List::isEmpty)) {
        return;
      }

      final double osScale = g2.getDeviceConfiguration().getDefaultTransform().getScaleX();
      boolean drewSomething = false;
      if (hexVisible) {
        drewSomething = drawHexOwnership(g2, osScale, sidePoints);
      }
      if (sideVisible && !drewSomething) {
        drawVoronoiAreas(g2, osScale, sidePoints);
      }
    }
    finally {
      g2.dispose();
    }
  }

  private void drawVoronoiAreas(Graphics2D g2, double osScale, EnumMap<Side, List<Point>> sidePoints) {
    final Rectangle2D bounds = computeBounds(sidePoints);
    if (bounds == null) {
      return;
    }

    final double scale = map.getZoom() * osScale;
    final AffineTransform transform = AffineTransform.getScaleInstance(scale, scale);
    final Area alliedArea = buildTerritoryArea(sidePoints.get(Side.ALLIES), sidePoints.get(Side.GERMANS), bounds);
    final Area germanArea = buildTerritoryArea(sidePoints.get(Side.GERMANS), sidePoints.get(Side.ALLIES), bounds);

    drawSideArea(g2, transform, alliedArea, Side.ALLIES);
    drawSideArea(g2, transform, germanArea, Side.GERMANS);
  }

  private boolean drawHexOwnership(Graphics2D g2, double osScale, EnumMap<Side, List<Point>> sidePoints) {
    boolean hasHexGrid = false;
    final Rectangle clipBounds = g2.getClipBounds();
    final Rectangle mapClip = clipBounds == null ? null : map.drawingToMap(new Rectangle(clipBounds), osScale);
    final double drawingScale = map.getZoom() * osScale;

    for (final Board board : map.getBoards()) {
      final HexGrid hexGrid = findHexGrid(board.getGrid());
      if (hexGrid == null) {
        continue;
      }
      hasHexGrid = true;
      colorizeBoardHexes(g2, board, hexGrid, sidePoints, mapClip, drawingScale);
    }

    return hasHexGrid;
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
                                  EnumMap<Side, List<Point>> sidePoints,
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
                                EnumMap<Side, List<Point>> sidePoints,
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
    final Side owner = findNearestSide(mapCenter, sidePoints);
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

  private Side findNearestSide(Point2D point, EnumMap<Side, List<Point>> sidePoints) {
    Side closest = null;
    double closestDistance = Double.POSITIVE_INFINITY;
    for (final Side side : Side.values()) {
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

  @Override
  public boolean drawAboveCounters() {
    return true;
  }

  @Override
  public void setup(boolean gameStarting) {
    // Reset to hidden whenever a game starts/stops.
    setBothVisible(false);
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
