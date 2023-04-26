/*
 *
 * Copyright (c) 2004-2012 by Rodney Kinney, Brent Easton
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
package VASSAL.build.module.map.boardPicker.board;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.map.boardPicker.board.mapgrid.ZoneHighlight;
import VASSAL.build.module.map.boardPicker.board.mapgrid.ZonedGridHighlighter;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;
import org.apache.commons.lang3.tuple.Pair;
import org.w3c.dom.Element;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Map Grid that contains any number of {@link VASSAL.build.module.map.boardPicker.board.mapgrid.Zone}s against a background {@link MapGrid}
 */
public class ZonedGrid extends AbstractConfigurable implements GeometricGrid, GridContainer {
  protected List<Zone> zones = new ArrayList<>();
  protected MapGrid background;
  protected GridContainer container;
  protected ZonedGridHighlighter zoneHighlighters;

  @Override
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[0];
  }

  @Override
  public String getAttributeValueString(String key) {
    return null;
  }

  @Override
  public void setAttribute(String key, Object value) {
  }

  @Override
  public Configurer getConfigurer() {
    return null;
  }

  @Override
  public void addTo(Buildable parent) {
    container = (GridContainer) parent;
    container.setGrid(this);
  }

  public GridContainer getContainer() {
    return container;
  }

  @Override
  public Dimension getSize() {
    return container.getSize();
  }

  @Override
  public boolean contains(Point p) {
    return container.contains(p);
  }

  @Override
  public void removeGrid(MapGrid grid) {
    if (background == grid) {
      background = null;
    }
  }

  @Override
  public Board getBoard() {
    return container != null ? container.getBoard() : null;
  }

  public Map getMap() {
    return getBoard() == null ? null : getBoard().getMap();
  }

  @Override
  public void setGrid(MapGrid grid) {
    background = grid;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return background == null ?
      new Class<?>[]{
        Zone.class,
        HexGrid.class,
        SquareGrid.class,
        RegionGrid.class
      } :
      new Class<?>[]{Zone.class};
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.MultiZoneGrid.component_type"); //$NON-NLS-1$
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ZonedGrid.html"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
    ((GridContainer) parent).removeGrid(this);
  }

  /*
   * Zones that do not use the background grid must clip out the portion of
   * the background grid that they cover. Cache as much of the work as
   * possible to prevent bogging down with large numbers of zones.
   */
  protected final java.util.Map<Pair<Point, Double>, Area> clipCache = new ConcurrentHashMap<>();

  protected Area makeUnscaledUntranslatedClipArea() {
    // construct unscaled area
    final Area a = new Area();
    for (final Zone zone : zones) {
      if (!zone.isUseParentGrid()) {
        a.add(new Area(zone.getShape()));
      }
    }

    return a.isEmpty() ? null : a;
  }

  protected Area makeClipArea(Pair<Point, Double> k) {
    final Area a = makeUnscaledUntranslatedClipArea();
    if (a != null) {
      // scale the clip area and move it to the anchor point for the board
      final Point p = k.getLeft();
      final double s = k.getRight();

      final AffineTransform t = AffineTransform.getScaleInstance(s, s);
      t.translate(p.x, p.y);
      a.transform(t);
    }

    return a;
  }

  @Override
  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {
    // Draw the background grid if there is a visible one
    if (background != null && background.isVisible()) {
      // Clip out the area covered by Zones not using the background grid
      final Graphics2D g2d = (Graphics2D) g;
      final Shape oldClip = g2d.getClip();
      if (oldClip != null) {
        final Area translatedZones = clipCache.computeIfAbsent(
          Pair.of(bounds.getLocation(), scale), this::makeClipArea
        );

        if (translatedZones != null) {
          final Area clipArea = new Area(oldClip);
          clipArea.subtract(translatedZones);
          g2d.setClip(clipArea);
        }
      }
      // Draw the clipped background grid
      background.draw(g, bounds, visibleRect, scale, reversed);
      g2d.setClip(oldClip);
    }

    /*
     * Draw each Zone
     */
    for (final Zone zone : zones) {
      zone.draw(g, bounds, visibleRect, scale, reversed);
    }
  }

  @Override
  public GridNumbering getGridNumbering() {
    return background != null ? background.getGridNumbering() : null;
  }

  @Override
  public Point getLocation(String location) throws BadCoords {
    for (final Zone zone : zones) {
      try {
        final Point p = zone.getLocation(location);
        if (p != null && zone.contains(p)) {
          return p;
        }
      }
      catch (final BadCoords bc) {
      }
    }
    if (background != null)
      return background.getLocation(location);
    else
      throw new BadCoords();
  }

  public Point getRegionLocation(String location) {
    for (final Zone zone : zones) {
      final Point p = zone.getRegionLocation(location);
      if (p != null && zone.contains(p)) {
        return p;
      }
    }
    return null;
  }

  @Override
  public boolean isVisible() {
    return true;
  }

  @Override
  public String locationName(Point p) {
    String name = null;
    for (final Zone zone : zones) {
      if (zone.contains(p)) {
        name = zone.locationName(p);
        break;
      }
    }
    if (name == null
        && background != null) {
      name = background.locationName(p);
    }
    return name;
  }

  @Override
  public String localizedLocationName(Point p) {
    String name = null;
    for (final Zone zone : zones) {
      if (zone.contains(p)) {
        name = zone.localizedLocationName(p);
        break;
      }
    }
    if (name == null
        && background != null) {
      name = background.localizedLocationName(p);
    }
    return name;
  }

  @Override
  public int range(Point p1, Point p2) {
    MapGrid grid = background;
    final Zone z1 = findZone(p1);
    final Zone z2 = findZone(p2);
    if (z1 == z2
      && z1 != null
      && z1.getGrid() != null) {
      grid = z1.getGrid();
    }
    return grid != null ? grid.range(p1, p2) : (int)Math.round(p1.distance(p2));
  }

  @Override
  public Area getGridShape(Point center, int range) {
    Area a = null;
    final Zone z = findZone(center);
    if (z != null
      && z.getGrid() instanceof GeometricGrid) {
      a = ((GeometricGrid)z.getGrid()).getGridShape(center, range);
    }
    if (a == null
      && background instanceof GeometricGrid) {
      a = ((GeometricGrid)background).getGridShape(center, range);
    }
    if (a == null) {
      a = new Area(new Ellipse2D.Double(center.x - range, center.y - range, range * 2, range * 2));
    }
    return a;
  }

  public Zone findZone(Point p) {
    for (final Zone zone : zones) {
      if (zone.contains(p)) {
        return zone;
      }
    }
    return null;
  }

  public Zone findZone(String name) {
    for (final Zone zone : zones) {
      if (zone.getName().equals(name)) {
        return zone;
      }
    }
    return null;
  }

  @Override
  public Point snapTo(Point p, boolean force) {
    Point snap = null;
    final Zone z = findZone(p);
    if (z != null) {
      snap = z.snapTo(p, force);
    }
    if (snap == null) {
      snap = background != null ? background.snapTo(p, force) : p;
    }
    return snap;
  }

  @Override
  public Point snapTo(Point p) {
    return snapTo(p, false);
  }

  @Override
  public boolean isLocationRestricted(Point p) {
    for (final Zone zone : zones) {
      if (zone.contains(p)) {
        return zone.getGrid() != null && zone.getGrid().isLocationRestricted(p);
      }
    }
    return background != null && background.isLocationRestricted(p);
  }

  public void addZone(Zone z) {
    zones.add(z);
  }

  public void removeZone(Zone z) {
    zones.remove(z);
  }

  public Iterator<Zone> getZones() {
    return zones.iterator();
  }

  public List<Zone> getZonesList() {
    return new ArrayList<>(zones);
  }

  public MapGrid getBackgroundGrid() {
    return background;
  }

  public void setBackgroundGrid(MapGrid background) {
    this.background = background;
  }

  @Override
  public void build(Element e) {
    super.build(e);
    if (getComponentsOf(ZonedGridHighlighter.class).isEmpty()) {
      addChild(new ZonedGridHighlighter());
    }
  }

  private void addChild(Buildable b) {
    add(b);
    b.addTo(this);
  }

  public void setZoneHighlighter(ZonedGridHighlighter zh) {
    zoneHighlighters = zh;
  }

  public ZoneHighlight getZoneHighlight(String name) {
    if (zoneHighlighters != null) {
      return zoneHighlighters.getZoneHighlightByName(name);
    }
    return null;
  }
}
