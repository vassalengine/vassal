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

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AbstractFolder;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.mapgrid.PolygonEditor;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.command.Command;
import VASSAL.tools.swing.SwingUtils;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.RenderingHints;

/**
 * Simple configurable polygon overlay that can be drawn on top of a {@link Map}.
 * Intended as a first pass at showing "front line" areas directly on the map canvas.
 */
public class FrontPolygon extends AbstractConfigurable implements GameComponent, Drawable {

  public static final String NAME = "name"; //NON-NLS
  public static final String COORDINATES = "coords"; //NON-NLS
  public static final String LINE_COLOR = "lineColor"; //NON-NLS
  public static final String FILL_COLOR = "fillColor"; //NON-NLS
  public static final String LINE_WIDTH = "lineWidth"; //NON-NLS
  public static final String FILL_ENABLED = "fillEnabled"; //NON-NLS
  public static final String ABOVE_COUNTERS = "drawAboveCounters"; //NON-NLS

  private static final String DEFAULT_COORDS = "50,50;100,20;150,50;100,100";
  private static final int DEFAULT_LINE_WIDTH = 3;

  private Map map;
  private Polygon polygon = new Polygon();
  private String coordValue = "";
  private Color lineColor = Color.RED;
  private Color fillColor = new Color(255, 0, 0, 80);
  private int lineWidth = DEFAULT_LINE_WIDTH;
  private boolean fillEnabled = true;
  private boolean drawAboveCounters = true;

  public FrontPolygon() {
    setConfigureName("Front Polygon");
    setCoordinates(DEFAULT_COORDS);
  }

  private boolean hasDrawablePolygon() {
    return polygon != null && polygon.npoints >= 3;
  }

  private void repaintMap() {
    if (map != null) {
      map.repaint();
    }
  }

  private void setCoordinates(String coords) {
    coordValue = coords == null ? "" : coords.trim();
    polygon = PolygonEditor.stringToPolygon(coordValue);
    if (polygon == null) {
      polygon = new Polygon();
    }
    repaintMap();
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (!hasDrawablePolygon()) {
      return;
    }

    final Graphics2D g2 = (Graphics2D) g.create();
    try {
      g2.addRenderingHints(SwingUtils.FONT_HINTS);
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

      if (fillEnabled && fillColor != null && fillColor.getAlpha() > 0) {
        g2.setColor(fillColor);
        g2.fillPolygon(polygon);
      }

      if (lineColor != null && lineWidth > 0) {
        g2.setColor(lineColor);
        g2.setStroke(new BasicStroke(lineWidth, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        g2.drawPolygon(polygon);
      }
    }
    finally {
      g2.dispose();
    }
  }

  @Override
  public boolean drawAboveCounters() {
    return drawAboveCounters;
  }

  @Override
  public void setup(boolean gameStarting) {
    repaintMap();
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  @Override
  public void addTo(Buildable parent) {
    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder) parent).getNonFolderAncestor();
    }

    if (!(parent instanceof Map)) {
      throw new IllegalStateException("FrontPolygon must be added to a Map"); //NON-NLS
    }

    map = (Map) parent;

    map.addDrawComponent(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  @Override
  public void removeFrom(Buildable parent) {
    if (parent instanceof AbstractFolder) {
      parent = ((AbstractFolder) parent).getNonFolderAncestor();
    }

    if (map != null) {
      map.removeDrawComponent(this);
    }
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    map = null;
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      NAME,
      COORDINATES,
      LINE_COLOR,
      FILL_COLOR,
      LINE_WIDTH,
      FILL_ENABLED,
      ABOVE_COUNTERS
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      "Name",
      "Coordinates (x,y;x,y;...)",
      "Line color",
      "Fill color",
      "Line width (pixels)",
      "Fill polygon",
      "Draw above counters"
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      Color.class,
      Color.class,
      Integer.class,
      Boolean.class,
      Boolean.class
    };
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (FILL_COLOR.equals(name)) {
      return () -> fillEnabled;
    }
    return super.getAttributeVisibility(name);
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName(value == null ? "" : value.toString());
    }
    else if (COORDINATES.equals(key)) {
      setCoordinates(value == null ? "" : value.toString());
    }
    else if (LINE_COLOR.equals(key)) {
      lineColor = asColor(value, lineColor);
      repaintMap();
    }
    else if (FILL_COLOR.equals(key)) {
      fillColor = asColor(value, fillColor);
      repaintMap();
    }
    else if (LINE_WIDTH.equals(key)) {
      lineWidth = asInt(value, DEFAULT_LINE_WIDTH);
      repaintMap();
    }
    else if (FILL_ENABLED.equals(key)) {
      fillEnabled = asBoolean(value, true);
      repaintMap();
    }
    else if (ABOVE_COUNTERS.equals(key)) {
      drawAboveCounters = asBoolean(value, true);
      repaintMap();
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (COORDINATES.equals(key)) {
      return coordValue;
    }
    else if (LINE_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(lineColor);
    }
    else if (FILL_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(fillColor);
    }
    else if (LINE_WIDTH.equals(key)) {
      return Integer.toString(lineWidth);
    }
    else if (FILL_ENABLED.equals(key)) {
      return Boolean.toString(fillEnabled);
    }
    else if (ABOVE_COUNTERS.equals(key)) {
      return Boolean.toString(drawAboveCounters);
    }
    return null;
  }

  private static Color asColor(Object value, Color fallback) {
    if (value instanceof Color) {
      return (Color) value;
    }
    else if (value instanceof String && !((String) value).isEmpty()) {
      return ColorConfigurer.stringToColor((String) value);
    }
    return fallback;
  }

  private static int asInt(Object value, int fallback) {
    if (value instanceof Number) {
      return ((Number) value).intValue();
    }
    if (value instanceof String && !((String) value).isEmpty()) {
      try {
        return Integer.parseInt(((String) value).trim());
      }
      catch (NumberFormatException ignore) {
        return fallback;
      }
    }
    return fallback;
  }

  private static boolean asBoolean(Object value, boolean fallback) {
    if (value instanceof Boolean) {
      return (Boolean) value;
    }
    if (value instanceof String && !((String) value).isEmpty()) {
      return Boolean.parseBoolean(((String) value).trim());
    }
    return fallback;
  }
}
