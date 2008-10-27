/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.Timer;

import org.jdesktop.animation.timing.Animator;
import org.jdesktop.animation.timing.TimingTargetAdapter;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.HexGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.RegionGrid;
import VASSAL.build.module.map.boardPicker.board.SquareGrid;
import VASSAL.build.module.map.boardPicker.board.ZonedGrid;
import VASSAL.build.module.map.boardPicker.board.mapgrid.GridContainer;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ErrorUtils;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.MissingImageException;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OpErrorDialog;
import VASSAL.tools.imageop.Repainter;
import VASSAL.tools.imageop.ScaleOp;
import VASSAL.tools.imageop.SourceOp;

public class Board extends AbstractConfigurable implements GridContainer {
  /**
   * A Board is a piece of a Map.
   * A Map can cantain a set of boards layed out in a rectangular grid.
   */
  public static final String NAME = "name";
  public static final String IMAGE = "image";
  public static final String WIDTH = "width";
  public static final String HEIGHT = "height";
  public static final String COLOR = "color";
  public static final String REVERSIBLE = "reversible";

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

  @Deprecated protected String boardName = "Board 1";
  @Deprecated protected Image boardImage;

  protected SourceOp boardImageOp;
  protected ScaleOp scaledImageOp;

  public Board() {
  }

  /** 
   * @return this <code>Board</code>'s {@link Map}.
   * Until a game is started that is using this board, the map will be null.
   */
  public Map getMap() {
    return map;
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

  public void addTo(Buildable b) {
    validator = new SingleChildInstance(this, MapGrid.class);
  }

  public void removeFrom(Buildable b) {
  }

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

  public String[] getAttributeDescriptions() {
    return new String[] {
      "Board name:  ",
      "Board image:  ",
      "Reversible:  ",
      "Board width:  ",
      "Board height:  ",
      "Background color:  "
    };
  }
  
  public static String getConfigureTypeName() {
    return "Board";
  }

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

  public VisibilityCondition getAttributeVisibility(String name) {
    if (REVERSIBLE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return imageFile != null;
        }
      };
    }
    else if (WIDTH.equals(name) || HEIGHT.equals(name) || COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return imageFile == null;
        }
      };
    }
    else {
      return null;
    }
  }

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

  public void setAttribute(String key, Object val) {
    if (NAME.equals(key)) {
      setConfigureName((String) val);
    }
    else if (IMAGE.equals(key)) {
      if (val instanceof File) {
        val = ((File) val).getName();
      }
      imageFile = (String) val;
      boardImageOp = imageFile == null || imageFile.trim().length() == 0
                   ? null : Op.loadLarge(imageFile);
    }
    else if (WIDTH.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      if (val != null) {
        boundaries.setSize(((Integer) val).intValue(), boundaries.height);
      }
    }
    else if (HEIGHT.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }
      if (val != null) {
        boundaries.setSize(boundaries.width, ((Integer) val).intValue());
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
      reversible = ((Boolean) val).booleanValue();
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] {
      HexGrid.class,
      SquareGrid.class,
      RegionGrid.class,
      ZonedGrid.class
    };
  }

  public void draw(Graphics g, int x, int y, double zoom, Component obs) {
    drawRegion(g,
               new Point(x,y),
               new Rectangle(x, y,
                             Math.round((float) zoom*boundaries.width),
                             Math.round((float) zoom*boundaries.height)),
               zoom, obs);
  }

  private ConcurrentMap<Point,Future<Image>> requested =
    new ConcurrentHashMap<Point,Future<Image>>();

  private java.util.Map<Point,Float> alpha =
    new ConcurrentHashMap<Point,Float>();

  private static Comparator<Point> tileOrdering = new Comparator<Point>() {
    public int compare(Point t1, Point t2) {
      if (t1.y < t2.y) return -1;
      if (t1.y > t2.y) return  1;
      return t1.x - t2.x;
    }
  };

  protected void drawTile(Graphics g, Future<Image> fim,
                          int tx, int ty, Component obs) {
    try {
      g.drawImage(fim.get(), tx, ty, obs);
    }
    catch (CancellationException e) {
      // FIXME: bug until we permit cancellation 
      ErrorDialog.bug(e);
    }
    catch (InterruptedException e) {
      ErrorDialog.bug(e);
    }
    catch (ExecutionException e) {
      final MissingImageException mie =
        ErrorUtils.getAncestorOfClass(MissingImageException.class, e);
      if (mie != null) {
        ErrorDialog.dataError(new BadDataReport(
          "Image not found", mie.getFile().getAbsolutePath(), mie));
      }
      else {
        ErrorDialog.bug(e);
      }
    }
  }

  public void drawRegion(final Graphics g,
                         final Point location,
                         Rectangle visibleRect,
                         double zoom,
                         final Component obs) {
    zoom *= magnification;
    final Rectangle bounds =
      new Rectangle(location.x, location.y,
                    Math.round(boundaries.width * (float) zoom),
                    Math.round(boundaries.height * (float) zoom));

    if (visibleRect.intersects(bounds)) {
      visibleRect = visibleRect.intersection(bounds);
      if (boardImageOp != null) {
        final ImageOp op;
        if (zoom == 1.0 && !reversed) {
          op = boardImageOp;
        }
        else {
          if (scaledImageOp == null || scaledImageOp.getScale() != zoom) {
            scaledImageOp = Op.scale(boardImageOp, zoom);
          }
          op = reversed ? Op.rotate(scaledImageOp, 180) : scaledImageOp;
        }

        final Rectangle r = new Rectangle(visibleRect.x - location.x,
                                          visibleRect.y - location.y,
                                          visibleRect.width,
                                          visibleRect.height);
        final int ow = op.getTileWidth();
        final int oh = op.getTileHeight();

        final Point[] tiles = op.getTileIndices(r);

        for (Point tile : tiles) {
          // find tile position
          final int tx = location.x + tile.x*ow;
          final int ty = location.y + tile.y*oh;

          // find actual tile size
          final int tw = Math.min(ow, location.x+bounds.width-tx);
          final int th = Math.min(oh, location.y+bounds.height-ty);
          
          final Repainter rep = obs == null ? null :
            new Repainter(obs, tx, ty, tw, th);

          try {
            final Future<Image> fim = op.getFutureTile(tile.x, tile.y, rep);

            if (obs == null) {
              drawTile(g, fim, tx, ty, obs);
            }
            else {
              if (fim.isDone()) {
                if (requested.containsKey(tile)) {
                  requested.remove(tile);
                  final Point t = tile;

                  final Animator a = new Animator(100,
                    new TimingTargetAdapter() {
                      @Override
                      public void timingEvent(float fraction) {
                        alpha.put(t, fraction);
                        obs.repaint(tx, ty, tw, th); 
                      }
                    }
                  );

                  a.setResolution(20);
                  a.start();
                }
                else {
                  Float a = alpha.get(tile);
                  if (a != null && a < 1.0f) {
                    final Graphics2D g2d = (Graphics2D) g;
                    final Composite oldComp = g2d.getComposite();
                    g2d.setComposite(
                      AlphaComposite.getInstance(AlphaComposite.SRC_OVER, a));
                    drawTile(g2d, fim, tx, ty, obs);
                    g2d.setComposite(oldComp);
                  }
                  else {
                    alpha.remove(tile);                   
                    drawTile(g, fim, tx, ty, obs);
                  } 
                }
              }
              else {
                requested.putIfAbsent(tile, fim);
              }
            }
          }
// FIXME: should getTileFuture() throw these? Yes, probably, because it's
// synchronous when obs is null.
          catch (CancellationException e) {
            // FIXME: bug until we permit cancellation
            ErrorDialog.bug(e);
          }
          catch (ExecutionException e) {
            // FIXME: bug until we figure out why getTileFuture() throws this
            ErrorDialog.bug(e);
          }
        }

        for (Point tile : requested.keySet().toArray(new Point[0])) {
          if (Arrays.binarySearch(tiles, tile, tileOrdering) < 0) {
            requested.remove(tile);
          }
        }

/*
        final StringBuilder sb = new StringBuilder();
        for (Point tile : requested.keySet().toArray(new Point[0])) {
          if (Arrays.binarySearch(tiles, tile, tileOrdering) < 0) {
            final Future<Image> fim = requested.remove(tile);
            if (!fim.isDone()) {
              sb.append("(")
                .append(tile.x)
                .append(",")
                .append(tile.y)
                .append(") ");
            }
          }
        }
        if (sb.length() > 0) {
          sb.insert(0, "cancelling: ").append("\n");
          System.out.print(sb.toString());
        }
*/
      }
      else {
        if (color != null) {
          g.setColor(color);
          g.fillRect(visibleRect.x, visibleRect.y,
                     visibleRect.width, visibleRect.height);
        }
        else {
          g.clearRect(visibleRect.x, visibleRect.y,
                      visibleRect.width, visibleRect.height);
        }
      }

      if (grid != null) {
        grid.draw(g, bounds, visibleRect, zoom, reversed);
      }
    }
  }

  @Deprecated
  public synchronized Image getScaledImage(double zoom, Component obs) {
    try {
      final ImageOp sop = Op.scale(boardImageOp, zoom);
      return (reversed ? Op.rotate(sop, 180) : sop).getImage(null);
    }
    catch (CancellationException e) {
      ErrorDialog.bug(e);
    }
    catch (InterruptedException e) {
      ErrorDialog.bug(e);
    }
    catch (ExecutionException e) {
      OpErrorDialog.error(e, boardImageOp);
    }
    return null;
  }

  public void setReversed(boolean val) {
    if (reversible) {
      if (reversed != val) {
        reversed = val;
        scaledImageOp = null;   // get a new rendered version on next paint
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
      p.x = (int) Math.round(p.x/magnification);
      p.y = (int) Math.round(p.y/magnification);
    }

    return p;
  }

  /**
   * If this board is reversed, return the location in reversed coordinates
   */
  public Point globalCoordinates(Point p) {
    if (magnification != 1.0) {
      p.x = (int) Math.round(p.x*magnification);
      p.y = (int) Math.round(p.y*magnification);
    }

    if (reversed) {
      p.x = bounds().width - p.x;
      p.y =  bounds().height - p.y;
    }

    return p;
  }

  public void setGrid(MapGrid mg) {
    grid = mg;
  }

  public void removeGrid(MapGrid grid) {
    if (this.grid == grid) {
      this.grid = null;
    }
  }

  public Board getBoard() {
    return this;
  }

  public Dimension getSize() {
    return bounds().getSize();
  }

  public MapGrid getGrid() {
    return grid;
  }

  public Board copy() {
    Board b = new Board();
    b.build(getBuildElement(Builder.createNewDocument()));
    return b;
  }

  /**
   * @deprecated Images are now fixed automagically using {@link ImageOp}s.
   */
  @Deprecated
  public void fixImage(Component map) { }

 /**
  * @deprecated Images are now fixed automagically using {@link ImageOp}s.
  */
  @Deprecated
  public void fixImage() { }

  public String locationName(Point p) {
    return grid == null ? null : grid.locationName(localCoordinates(p));
  }

  public String localizedLocationName(Point p) {
    return grid == null ? null : grid.localizedLocationName(localCoordinates(p));
  }
  
  public Point snapTo(Point p) {
    return grid == null ? p : globalCoordinates(grid.snapTo(localCoordinates(p)));
  }

  /**
   * @return true if the given point may not be a local location.
   * I.e., if this grid will attempt to snap it to the nearest grid location.
   */
  public boolean isLocationRestricted(Point p) {
    return grid == null ? false : grid.isLocationRestricted(localCoordinates(p));
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
        boundaries.setSize((int)Math.round(magnification*boundaries.width),
                           (int)Math.round(magnification*boundaries.height));
      }

      fixedBoundaries = true;
    }
    return new Rectangle(boundaries);
  }

  /**
   * @deprecated Bounds are now fixed automagically by {@link ImageOp}s. 
   */
  @Deprecated
  protected void fixBounds() { }

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

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Board.htm");
  }

  /**
   * Removes board images from the {@link VASSAL.tools.DataArchive} cache
   * @deprecated Board images are removed automatically now, when under
   * memory pressure.
   */
  @Deprecated
  public void cleanUp() {
    if (imageFile != null) {
      GameModule.getGameModule().getDataArchive().unCacheImage("images/" + imageFile);
    }
    if (boardImage != null) {
      GameModule.getGameModule().getDataArchive().unCacheImage(boardImage);
      boardImage = null;
    }
  }

  /**
   * Cleans up {@link Board}s (by invoking {@link Board#cleanUp}) when a
   * game is closed
   * @deprecated Only used to cleanup <code>Board</code> images, which
   * is now handled automatically by the cache. 
   */
  @Deprecated
  public static class Cleanup implements GameComponent {
    private static Cleanup instance;
    private Set<Board> toClean = new HashSet<Board>();
    private boolean gameStarted = false;

    public static void init() {
      if (instance == null) {
        instance = new Cleanup();
      }
    }

    private Cleanup() {
      GameModule.getGameModule().getGameState().addGameComponent(this);
    }

    public static Cleanup getInstance() {
      return instance;
    }

    /**
     * Mark this board as needing to be cleaned up when the game is closed
     * 
     * @param b
     */
    public void addBoard(Board b) {
      toClean.add(b);
    }

    public Command getRestoreCommand() {
      return null;
    }

    public void setup(boolean gameStarting) {
      if (gameStarted && !gameStarting) {
        for (Board board : toClean) {
          board.cleanUp();
        }
        toClean.clear();
      }
      gameStarted = gameStarting;
    }
  }

  public double getMagnification() {
    return magnification;
  }

  public void setMagnification(double magnification) {
    this.magnification = magnification;
    fixedBoundaries = false;
    bounds();
  }
}
