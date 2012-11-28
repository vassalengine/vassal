/*
 * $Id$
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

import org.w3c.dom.Element;

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

/**
 * Map Grid that contains any number of {@link VASSAL.build.module.map.boardPicker.board.mapgrid.Zone}s against a background {@link MapGrid}
 */
public class ZonedGrid extends AbstractConfigurable implements GeometricGrid, GridContainer {
  protected List<Zone> zones = new ArrayList<Zone>();
  protected MapGrid background;
  protected GridContainer container;
  protected ZonedGridHighlighter zoneHighlighters;

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object value) {
  }

  public Configurer getConfigurer() {
    return null;
  }

  public void addTo(Buildable parent) {
    container = (GridContainer) parent;
    container.setGrid(this);
  }

  public GridContainer getContainer() {
    return container;
  }

  public Dimension getSize() {
    return container.getSize();
  }

  public boolean contains(Point p) {
    return container.contains(p);
  }

  public void removeGrid(MapGrid grid) {
    if (background == grid) {
      background = null;
    }
  }

  public Board getBoard() {
    return container != null ? container.getBoard() : null;
  }

  public Map getMap() {
    return getBoard() == null ? null : getBoard().getMap();
  }

  public void setGrid(MapGrid grid) {
    background = grid;
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return background == null ? new Class<?>[]{Zone.class, HexGrid.class, SquareGrid.class, RegionGrid.class}
        : new Class<?>[]{Zone.class};
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.MultiZoneGrid.component_type"); //$NON-NLS-1$
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ZonedGrid.htm"); //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
    ((GridContainer) parent).removeGrid(this);
  }

  /*
   * Zones that do not use the background grid must clip out the portion of
   * the background grid that they cover. Cache as much of the work as
   * possible to prevent bogging down with large numbers of zones.
   */
  protected Area scaledZones = null;
  protected Area translatedZones = null;
  protected AffineTransform scaleTransform;
  protected AffineTransform translateTransform;
  protected double lastScale = 0.0;
  protected int lastX = -1;
  protected int lastY = -1;

  public void draw(Graphics g, Rectangle bounds, Rectangle visibleRect, double scale, boolean reversed) {

    /*
     * Skip clipping if there is no background grid, or it isn't visible
     */
    if (background != null && background.isVisible()) {

      /*
       * Calculate and cache scaled shape consisting of all zones that do not
       * use the parent grid. (There may be none!)
       */
      if (lastScale != scale || scaleTransform == null) {
        scaleTransform = AffineTransform.getScaleInstance(scale, scale);
        scaledZones = null;

        for (Zone zone : zones) {
          if (!zone.isUseParentGrid()) {
            if (scaledZones == null) {
              scaledZones = new Area(
                scaleTransform.createTransformedShape(zone.getShape()));
            }
            else {
              scaledZones.add(new Area(
                scaleTransform.createTransformedShape(zone.getShape())));
            }
          }
        }
        lastScale = scale;
        translateTransform = null;  // Force translatedZones to be regenerated
      }

      /*
       * Translate and cache the combined zone shape
       */
      if (lastX != bounds.x || lastY != bounds.y || translateTransform == null) {
        translateTransform = AffineTransform.getTranslateInstance(bounds.x, bounds.y);
        translatedZones = null;
        if (scaledZones != null) {
          translatedZones = new Area(translateTransform.createTransformedShape(scaledZones));
        }
        lastX = bounds.x;
        lastY = bounds.y;
      }

      /*
       * Clip out the area covered by the Zones not using the background grid and draw it.
       */
      Graphics2D g2d = (Graphics2D) g;
      Shape oldClip = g2d.getClip();
      if (translatedZones != null && oldClip != null) {
        Area clipArea = new Area(oldClip);
        clipArea.subtract(translatedZones);
        g2d.setClip(clipArea);
      }
      background.draw(g, bounds, visibleRect, scale, reversed);
      g2d.setClip(oldClip);
    }
    /*
     * Draw each Zone
     */
    for (Zone zone : zones) {
      zone.draw(g, bounds, visibleRect, scale, reversed);
    }
  }

  public GridNumbering getGridNumbering() {
    return background != null ? background.getGridNumbering() : null;
  }

  public Point getLocation(String location) throws BadCoords {
    for (Zone zone : zones) {
      try {
        Point p = zone.getLocation(location);
        if (p != null && zone.contains(p)) {
          return p;
        }
      }
      catch (BadCoords bc) {
      }
    }
    if (background != null)
      return background.getLocation(location);
    else
      throw new BadCoords();
  }

  public boolean isVisible() {
    return true;
  }

  public String locationName(Point p) {
    String name = null;
    for (Zone zone : zones) {
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

  public String localizedLocationName(Point p) {
    String name = null;
    for (Zone zone : zones) {
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

  public int range(Point p1, Point p2) {
    MapGrid grid = background;
    Zone z1 = findZone(p1);
    Zone z2 = findZone(p2);
    if (z1 == z2
      && z1 != null
      && z1.getGrid() != null) {
      grid = z1.getGrid();
    }
    return grid != null ? grid.range(p1, p2) : (int)Math.round(p1.distance(p2));
  }

  public Area getGridShape(Point center, int range) {
    Area a = null;
    Zone z = findZone(center);
    if (z != null
      && z.getGrid() instanceof GeometricGrid) {
      a = ((GeometricGrid)z.getGrid()).getGridShape(center,range);
    }
    if (a == null
      && background instanceof GeometricGrid) {
      a = ((GeometricGrid)background).getGridShape(center,range);
    }
    if (a == null) {
      a = new Area(new Ellipse2D.Double(center.x-range, center.y-range, range * 2, range * 2));
    }
    return a;
  }

  public Zone findZone(Point p) {
    for (Zone zone : zones) {
      if (zone.contains(p)) {
        return zone;
      }
    }
    return null;
  }

  public Zone findZone(String name) {
    for (Zone zone : zones) {
      if (zone.getName().equals(name)) {
        return zone;
      }
    }
    return null;
  }

  public Point snapTo(Point p) {
    Point snap = null;
    Zone z = findZone(p);
    if (z != null) {
      snap = z.snapTo(p);
    }
    if (snap == null) {
      snap = background != null ? background.snapTo(p) : p;
    }
    return snap;
  }

  public boolean isLocationRestricted(Point p) {
    for (Zone zone : zones) {
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

  public MapGrid getBackgroundGrid() {
    return background;
  }

  public void setBackgroundGrid(MapGrid background) {
    this.background = background;
  }

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
