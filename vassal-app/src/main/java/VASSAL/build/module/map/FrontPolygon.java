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

import javax.swing.JButton;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Simple runtime-only polygon overlay that can be toggled via a toolbar button or console command.
 */
public class FrontPolygon implements GameComponent, Drawable {
  private static final int[] X_POINTS = { 50, 100, 150, 100 };
  private static final int[] Y_POINTS = { 50, 20, 50, 100 };
  private static final Color OUTLINE_COLOR = Color.RED;
  private static final Color FILL_COLOR = new Color(255, 0, 0, 80);
  private static final int LINE_WIDTH = 3;

  private static final Set<FrontPolygon> INSTANCES = ConcurrentHashMap.newKeySet();

  private final Map map;
  private final Polygon polygon;
  private final JButton toggleButton;

  private boolean visible;

  public FrontPolygon(Map map) {
    this.map = map;
    this.polygon = new Polygon(X_POINTS, Y_POINTS, X_POINTS.length);

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

      if (FILL_COLOR.getAlpha() > 0) {
        g2.setColor(FILL_COLOR);
        g2.fillPolygon(polygon);
      }

      g2.setColor(OUTLINE_COLOR);
      g2.setStroke(new BasicStroke(LINE_WIDTH, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
      g2.drawPolygon(polygon);
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
}
