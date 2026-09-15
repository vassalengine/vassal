/*
 * Copyright (c) 2000-2026 by Rodney Kinney, Brent Easton, Joel Uckelman
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
package VASSAL.build.module.map.boardPicker;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.RegionGrid;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageTileSource;
import VASSAL.tools.image.TileRenderer;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.ScaleOp;
import VASSAL.tools.imageop.SourceOp;

import org.w3c.dom.Element;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Future;

public class Board extends AbstractConfigurable implements GridContainer {
  /**
   * A Board is a piece of a Map.
   * A Map can cantain a set of boards layed out in a rectangular grid.
   */
  public static final String NAME = "name"; //NON-NLS
  public static final String IMAGE = "image"; //NON-NLS
  public static final String WIDTH = "width"; //NON-NLS
  public static final String HEIGHT = "height"; //NON-NLS
  public static final String COLOR = "color"; //NON-NLS
  public static final String REVERSIBLE = "reversible"; //NON-NLS

  protected Point pos = new Point(0, 0);
  protected Rectangle boundaries = new Rectangle(0, 0, 500, 500);
  protected String imageFile;
  protected boolean reversible = false;
  protected boolean reversed = false;
  protected boolean fixedBoundaries = false;
  protected Color color = null;
  protected MapGrid grid = null;
  protected Map map;
  protected double magnification = 1.0;

  protected boolean cacheGrid = true;

  protected SourceOp boardImageOp;
  protected ScaleOp scaledImageOp;

  /**
   * @return this <code>Board</code>'s {@link Map}.
   * Until a game is started that is using this board, the map will be null.
   */
  public Map getMap() {
    return map;
  }

  public void setCacheGrid(boolean cg) {
    cacheGrid = cg;
  }

  public void setMap(Map map) {
    this.map = map;
  }

  public String getLocalizedName() {
    final String s = getLocalizedConfigureName();
    return s != null ? s : "";
  }

  public String getName() {
    final String s = getConfigureName();
    return s != null ? s : "";
  }

  @Override
  public void build(Element e) {
    super.build(e);
    if (e == null) {
      setConfigureName(Resources.getString("Editor.Board.component_type"));
    }
  }

  @Override
  public void addTo(Buildable b) {
    validator = new SingleChildInstance(this, MapGrid.class);
  }

  @Override
  public void removeFrom(Buildable b) {
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {
      NAME,
      IMAGE,
      REVERSIBLE,
      WIDTH,
      HEIGHT,
      COLOR
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
        Resources.getString(Resources.NAME_LABEL),
        Resources.getString("Editor.Board.image"), //$NON-NLS-1$
        Resources.getString("Editor.Board.reverse"), //$NON-NLS-1$
        Resources.getString("Editor.Board.width"), //$NON-NLS-1$
        Resources.getString("Editor.Board.height"), //$NON-NLS-1$
        Resources.getString(Resources.COLOR_LABEL),
    };
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Board.component_type"); //$NON-NLS-1$
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      Image.class,
      Boolean.class,
      Integer.class,
      Integer.class,
      Color.class
    };
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (REVERSIBLE.equals(name)) {
      return () -> imageFile != null;
    }
    else if (List.of(WIDTH, HEIGHT, COLOR).contains(name)) {
      return () -> imageFile == null;
    }
    else {
      return null;
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (IMAGE.equals(key)) {
      return imageFile;
    }
    else if (WIDTH.equals(key)) {
      return imageFile == null ? String.valueOf(boundaries.width) : null;
    }
    else if (HEIGHT.equals(key)) {
      return imageFile == null ? String.valueOf(boundaries.height) : null;
    }
    else if (COLOR.equals(key)) {
      return imageFile == null ? ColorConfigurer.colorToString(color) : null;
    }
    else if (REVERSIBLE.equals(key)) {
      return String.valueOf(reversible);
    }
    return null;
  }

  @Override
  public void setAttribute(String key, Object val) {
    if (NAME.equals(key)) {
      setConfigureName((String) val);
    }
    else if (IMAGE.equals(key)) {
      if (val instanceof File) {
        val = ((File) val).getName();
      }
      imageFile = (String) val;

      if (imageFile == null || imageFile.isBlank()) {
        boardImageOp = null;
      }
      else {
        final ImageTileSource ts =
          GameModule.getGameModule().getImageTileSource();

        boolean tiled = false;
        try {
          tiled = ts.tileExists("images/" + imageFile, 0, 0, 1.0);  //NON-NLS
        }
        catch (final ImageIOException e) {
          // ignore, not tiled
        }

        if (tiled) {
          boardImageOp = Op.loadLarge(imageFile);
        }
        else {
          boardImageOp = Op.load(imageFile);
        }
      }
    }
    else if (WIDTH.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String) val);
      }
      if (val != null) {
        boundaries.setSize((Integer) val, boundaries.height);
      }
    }
    else if (HEIGHT.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String) val);
      }
      if (val != null) {
        boundaries.setSize(boundaries.width, (Integer) val);
      }
    }
    else if (COLOR.equals(key)) {
      if (val instanceof String) {
        val = ColorConfigurer.stringToColor((String) val);
      }
      color = (Color) val;
    }
    else if (REVERSIBLE.equals(key)) {
      if (val instanceof String) {
        val = Boolean.valueOf((String) val);
      }
      reversible = (Boolean) val;
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] {
      HexGrid.class,
      SquareGrid.class,
      RegionGrid.class,
      ZonedGrid.class
    };
  }

  public void draw(Graphics g, int x, int y, double zoom, Component obs) {
    drawRegion(g,
               new Point(x, y),
               new Rectangle(x, y,
                             Math.round((float) zoom * boundaries.width),
                             Math.round((float) zoom * boundaries.height)),
               zoom, obs);
  }

  protected void drawTile(Graphics g, Future<BufferedImage> fim,
                          int tx, int ty, Component obs) {
    renderer.drawTile(g, fim, tx, ty, obs);
  }

  public void drawRegion(final Graphics g,
                         final Point location,
                         Rectangle visibleRect,
                         double zoom,
                         final Component obs) {
    drawRegion2D(g, location, visibleRect, zoom, obs);
  }

  private final TileRenderer renderer = new TileRenderer();

  public void drawRegion2D(final Graphics g,
                           final Point2D location,
                           Rectangle visibleRect,
                           double zoom,
                           final Component obs) {
    scaledImageOp = renderer.renderRegion(
      g,
      location,
      visibleRect,
      zoom,
      boundaries,
      magnification,
      boardImageOp,
      scaledImageOp,
      color,
      reversed,
      cacheGrid,
      grid,
      map.getView(),
      obs
    );
  }

  public void setReversed(boolean val) {
    if (reversible) {
      if (reversed != val) {
        reversed = val;
        // get a new rendered version on next paint
        scaledImageOp = null;
      }
    }
  }

  public boolean isReversed() {
    return reversed;
  }

  /**
   * If this board is reversed, return the location in un-reversed coordinates
   */
  public Point localCoordinates(Point p) {
    if (reversed) {
      p.x = bounds().width - p.x;
      p.y = bounds().height - p.y;
    }

    if (magnification != 1.0) {
      p.x = (int) Math.round(p.x / magnification);
      p.y = (int) Math.round(p.y / magnification);
    }

    return p;
  }

  /**
   * If this board is reversed, return the location in reversed coordinates
   */
  public Point globalCoordinates(Point p) {
    if (magnification != 1.0) {
      p.x = (int) Math.round(p.x * magnification);
      p.y = (int) Math.round(p.y * magnification);
    }

    if (reversed) {
      p.x = bounds().width - p.x;
      p.y =  bounds().height - p.y;
    }

    return p;
  }

  @Override
  public void setGrid(MapGrid mg) {
    grid = mg;
  }

  @Override
  public void removeGrid(MapGrid grid) {
    if (this.grid == grid) {
      this.grid = null;
    }
  }

  @Override
  public Board getBoard() {
    return this;
  }

  @Override
  public Dimension getSize() {
    return bounds().getSize();
  }

  @Override
  public boolean contains(Point p) {
    return bounds().contains(p);
  }

  public MapGrid getGrid() {
    return grid;
  }

  public Board copy() {
    final Board b = new Board();
    b.build(getBuildElement(Builder.createNewDocument()));
    return b;
  }

  public String locationName(Point p) {
    return grid == null ? null : grid.locationName(localCoordinates(p));
  }

  public String localizedLocationName(Point p) {
    return grid == null ? null : grid.localizedLocationName(localCoordinates(p));
  }

  /**
   * @see MapGrid#snapTo(Point p, boolean force, boolean onlyCenter)
   */
  public Point snapTo(Point p, boolean force, boolean onlyCenter) {
    return grid == null ? p : globalCoordinates(grid.snapTo(localCoordinates(p), force, onlyCenter));
  }

  /**
   * @see MapGrid#snapTo(Point p, boolean force, boolean onlyCenter)
   */
  public Point snapTo(Point p, boolean force) {
    return snapTo(p, force, false);
  }

  /**
   * @see Board#snapTo(Point p, boolean force, boolean onlyCenter)
   */
  public Point snapTo(Point p) {
    return snapTo(p, false, false);
  }

    /**
     * @return true if the given point may not be a local location.
     * I.e., if this grid will attempt to snap it to the nearest grid location.
     */
  public boolean isLocationRestricted(Point p) {
    return grid != null && grid.isLocationRestricted(localCoordinates(p));
  }

  public String fileName() {
    return imageFile;
  }

  /**
   * @return Position of this board relative to the other boards (0,0) is the upper left, (0,1) is to the right, etc.
   */
  public Point relativePosition() {
    return pos;
  }

  /**
   * @return The (read-only) boundaries of this Board within the overall Map
   */
  public Rectangle bounds() {
    if (imageFile != null && boardImageOp != null && !fixedBoundaries) {
      boundaries.setSize(boardImageOp.getSize());

      if (magnification != 1.0) {
        boundaries.setSize((int)Math.round(magnification * boundaries.width),
                           (int)Math.round(magnification * boundaries.height));
      }

      fixedBoundaries = true;
    }
    return new Rectangle(boundaries);
  }

  /**
   * Translate the location of the board by the given number of pixels
   *
   * @see #bounds()
   */
  public void translate(int x, int y) {
    boundaries.translate(x, y);
  }

  /**
   * Set the location of this board
   *
   * @see #bounds()
   */
  public void setLocation(int x, int y) {
    boundaries.setLocation(x, y);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Board.html"); //NON-NLS
  }

  public double getMagnification() {
    return magnification;
  }

  public void setMagnification(double magnification) {
    this.magnification = magnification;
    fixedBoundaries = false;
    bounds();
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (imageFile != null) s.add(imageFile);
  }
}
