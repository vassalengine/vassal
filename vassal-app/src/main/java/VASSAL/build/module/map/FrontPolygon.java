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
import java.awt.geom.Path2D;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.HashSet;
import java.util.Comparator;

/**
 * Simple runtime-only polygon overlay that can be toggled via a toolbar button or console command.
 */
public class FrontPolygon implements GameComponent, Drawable {
  private static final float LINE_WIDTH = 3f;
  private static final int MIN_MARKER_RADIUS = 8;
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

    final Graphics2D g2 = (Graphics2D) g.create();
    try {
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

      final double osScale = g2.getDeviceConfiguration().getDefaultTransform().getScaleX();
      final EnumMap<Side, List<Point>> sidePoints = collectSidePoints();

      for (final Side side : Side.values()) {
        drawSide(g2, osScale, sidePoints.get(side), side);
      }
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

  private void drawSide(Graphics2D g2, double osScale, List<Point> rawPoints, Side side) {
    if (rawPoints == null || rawPoints.isEmpty()) {
      return;
    }

    final List<Point> points = new ArrayList<>(rawPoints);

    if (points.size() >= 3) {
      final List<Point> hull = computeConvexHull(points);
      if (hull.size() >= 3) {
        drawPolygon(g2, osScale, hull, side);
        return;
      }
      else if (hull.size() == 2) {
        drawLine(g2, osScale, hull.get(0), hull.get(1), side);
        return;
      }
      else if (hull.size() == 1) {
        drawMarker(g2, osScale, hull.get(0), side);
        return;
      }
    }

    if (points.size() == 2) {
      drawLine(g2, osScale, points.get(0), points.get(1), side);
    }
    else {
      drawMarker(g2, osScale, points.get(0), side);
    }
  }

  private void drawPolygon(Graphics2D g2, double osScale, List<Point> hull, Side side) {
    final Path2D path = buildPath(hull, osScale);
    g2.setColor(side.fillColor);
    g2.fill(path);
    g2.setColor(side.outlineColor);
    g2.setStroke(OUTLINE_STROKE);
    g2.draw(path);
  }

  private void drawLine(Graphics2D g2, double osScale, Point start, Point end, Side side) {
    final Point p1 = map.mapToDrawing(start, osScale);
    final Point p2 = map.mapToDrawing(end, osScale);
    g2.setColor(side.outlineColor);
    g2.setStroke(OUTLINE_STROKE);
    g2.drawLine(p1.x, p1.y, p2.x, p2.y);
  }

  private void drawMarker(Graphics2D g2, double osScale, Point position, Side side) {
    final Point drawPoint = map.mapToDrawing(position, osScale);
    final int radius = Math.max(1, (int) Math.round(MIN_MARKER_RADIUS * osScale));
    final int diameter = radius * 2;
    g2.setColor(side.fillColor);
    g2.fillOval(drawPoint.x - radius, drawPoint.y - radius, diameter, diameter);
    g2.setColor(side.outlineColor);
    g2.setStroke(OUTLINE_STROKE);
    g2.drawOval(drawPoint.x - radius, drawPoint.y - radius, diameter, diameter);
  }

  private Path2D buildPath(List<Point> hull, double osScale) {
    final Path2D path = new Path2D.Double();
    for (int i = 0; i < hull.size(); i++) {
      final Point drawPoint = map.mapToDrawing(hull.get(i), osScale);
      if (i == 0) {
        path.moveTo(drawPoint.x, drawPoint.y);
      }
      else {
        path.lineTo(drawPoint.x, drawPoint.y);
      }
    }
    path.closePath();
    return path;
  }

  private List<Point> computeConvexHull(List<Point> sourcePoints) {
    final List<Point> points = new ArrayList<>(sourcePoints);
    points.sort(Comparator.comparingInt((Point p) -> p.x).thenComparingInt(p -> p.y));

    final List<Point> lower = new ArrayList<>();
    for (final Point point : points) {
      while (lower.size() >= 2 && cross(lower.get(lower.size() - 2), lower.get(lower.size() - 1), point) <= 0) {
        lower.remove(lower.size() - 1);
      }
      lower.add(point);
    }

    final List<Point> upper = new ArrayList<>();
    for (int i = points.size() - 1; i >= 0; --i) {
      final Point point = points.get(i);
      while (upper.size() >= 2 && cross(upper.get(upper.size() - 2), upper.get(upper.size() - 1), point) <= 0) {
        upper.remove(upper.size() - 1);
      }
      upper.add(point);
    }

    if (!lower.isEmpty()) {
      lower.remove(lower.size() - 1);
    }
    if (!upper.isEmpty()) {
      upper.remove(upper.size() - 1);
    }

    lower.addAll(upper);
    return lower;
  }

  private long cross(Point o, Point a, Point b) {
    final long x1 = a.x - o.x;
    final long y1 = a.y - o.y;
    final long x2 = b.x - o.x;
    final long y2 = b.y - o.y;
    return x1 * y2 - y1 * x2;
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
