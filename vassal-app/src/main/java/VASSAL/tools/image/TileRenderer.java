/*
 * Copyright (c) 2012-2026 by Joel Uckelman
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

package VASSAL.tools.image;

import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.board.GridOp;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.SolidColorOp;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.imageop.FixedScaleOpBitmapImpl;
import VASSAL.tools.imageop.FixedScaleOpTiledBitmapImpl;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.Repainter;
import VASSAL.tools.imageop.ScaleOp;
import VASSAL.tools.imageop.SourceOp;
import VASSAL.tools.imageop.SourceOpTiledBitmapImpl;
import VASSAL.tools.imageop.SVGOp;

import org.jdesktop.animation.timing.Animator;
import org.jdesktop.animation.timing.TimingTargetAdapter;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

public class TileRenderer {
  private static final Color CLEAR = new Color(0, 0, 0, 0);

  private final ConcurrentMap<Point, Future<BufferedImage>> requested = new ConcurrentHashMap<>();

  private final java.util.Map<Point, Float> alpha = new ConcurrentHashMap<>();

  private final ConcurrentMap<Point, Future<BufferedImage>> o_requested = new ConcurrentHashMap<>();

  private static final Comparator<Point> tileOrdering = (t1, t2) -> {
    if (t1.y < t2.y) return -1;
    if (t1.y > t2.y) return 1;
    return t1.x - t2.x;
  };

  private ScaleOp scaledImageOp = null;

  public void regenerateScaledImageOp() {
    scaledImageOp = null;
  }

  public void drawTile(Graphics g, Future<BufferedImage> fim,
                          int tx, int ty, Component obs) {
    try {
      g.drawImage(fim.get(), tx, ty, obs);
    }
    catch (final CancellationException e) {
      // FIXME: bug until we permit cancellation
      ErrorDialog.bug(e);
    }
    catch (final InterruptedException e) {
      // This happens if taking a snapshot of the map is cancelled.
      // FIXME: Can we handle this in ImageSaver instead?
    }
    catch (final ExecutionException e) {
      if (!Op.handleException(e)) ErrorDialog.bug(e);
    }
  }

  public void renderRegion(
    final Graphics g,
    final Point2D location,
    Rectangle visibleRect,
    double zoom,
    final Rectangle boundaries,
    double magnification,
    final SourceOp boardImageOp,
    final Color color,
    boolean reversed,
    boolean cacheGrid,
    final MapGrid grid,
    final Map map,
    final Component obs
  ) {

    final int lx = (int) Math.floor(location.getX());
    final int ly = (int) Math.floor(location.getY());

    final Rectangle bounds = new Rectangle(
      lx,
      ly,
      (int) Math.floor(location.getX() + boundaries.width * zoom) - lx,
      (int) Math.floor(location.getY() + boundaries.height * zoom) - ly
    );

    if (!visibleRect.intersects(bounds)) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();

    visibleRect = visibleRect.intersection(bounds);

    // location and boundaries already have magnification applied to them,
    // so don't adjust the zoom for magnification until after we've set
    // the bounds.
    zoom *= magnification;

    ImageOp op;
    if (boardImageOp != null) {
      if (zoom == 1.0 && !reversed) {
        op = boardImageOp;
      }
      else {
        if (scaledImageOp == null || scaledImageOp.getScale() != zoom) {
          if (boardImageOp instanceof SVGOp) {
            scaledImageOp = Op.scale(boardImageOp, zoom);
          }
          else if (boardImageOp instanceof SourceOpTiledBitmapImpl) {
            scaledImageOp = new FixedScaleOpTiledBitmapImpl(boardImageOp, zoom, bounds.width, bounds.height);
          }
          else {
            scaledImageOp = new FixedScaleOpBitmapImpl(boardImageOp, zoom, bounds.width, bounds.height);
          }
        }
        op = reversed ? Op.rotate(scaledImageOp, 180) : scaledImageOp;
      }
    }
    else {
      op = new SolidColorOp(color == null ? CLEAR : color, bounds.width, bounds.height);
    }

    if (cacheGrid && grid != null) {
      op = new GridOp(op, grid, zoom, reversed, g2d.getRenderingHints());
    }

    final Rectangle r = new Rectangle(visibleRect.x - lx,
                                      visibleRect.y - ly,
                                      visibleRect.width,
                                      visibleRect.height);
    final int ow = op.getTileWidth();
    final int oh = op.getTileHeight();

    final Point[] tiles = op.getTileIndices(r);
    for (final Point tile : tiles) {
      // find tile position
      final int tx = lx + tile.x * ow;
      final int ty = ly + tile.y * oh;

      // find actual tile size
      final int tw = Math.min(ow, lx + bounds.width - tx);
      final int th = Math.min(oh, ly + bounds.height - ty);

      // find position in component
      final int cx = (int)(tx / os_scale);
      final int cy = (int)(ty / os_scale);

      // find tile size in component
      final int cw = (int) Math.ceil(tw / os_scale);
      final int ch = (int) Math.ceil(th / os_scale);

      final Repainter rep = obs == null ? null :
        new Repainter(obs, cx, cy, cw, ch);

      try {
        final Future<BufferedImage> fim =
          op.getFutureTile(tile.x, tile.y, rep);

        if (obs == null) {
          drawTile(g, fim, tx, ty, obs);
        }
        else {
          if (fim.isDone()) {
// FIXME: We check whether the observer here is a map view in order to
// avoid mixing requests (and fade-in) between maps and their overview
// maps. This is a kludge which should be fixed when model-view
// separation happens.
            if (map != null && obs == map.getView()) {
              if (requested.containsKey(tile)) {
                requested.remove(tile);
                final Point t = tile;

                final Animator a = new Animator(100,
                  new TimingTargetAdapter() {
                    @Override
                    public void timingEvent(float fraction) {
                      alpha.put(t, fraction);
                      obs.repaint(cx, cy, cw, ch);
                    }
                  }
                );

                a.setResolution(20);
                a.start();
              }
              else {
                final Float a = alpha.get(tile);
                if (a != null && a < 1.0f) {
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
              if (o_requested.containsKey(tile)) {
                o_requested.remove(tile);
                obs.repaint(cx, cy, cw, ch);
              }
              else {
                drawTile(g, fim, tx, ty, obs);
              }
            }
          }
          else {
            if (map != null && obs == map.getView()) {
              requested.putIfAbsent(tile, fim);
            }
            else {
              o_requested.putIfAbsent(tile, fim);
            }
          }
        }
      }
// FIXME: should getTileFuture() throw these? Yes, probably, because it's
// synchronous when obs is null.
      catch (final CancellationException | ExecutionException e) {
        // FIXME: bug until we permit cancellation
        // FIXME: bug until we figure out why getTileFuture() throws ExecutionException
        ErrorDialog.bug(e);
      }
    }

    if (map != null && obs == map.getView()) {
      for (final Point tile : requested.keySet().toArray(new Point[0])) {
        if (Arrays.binarySearch(tiles, tile, tileOrdering) < 0) {
          requested.remove(tile);
        }
      }
    }
    else {
      for (final Point tile : o_requested.keySet().toArray(new Point[0])) {
        if (Arrays.binarySearch(tiles, tile, tileOrdering) < 0) {
          o_requested.remove(tile);
        }
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

    if (!cacheGrid && grid != null) {
      grid.draw(g, bounds, visibleRect, zoom, reversed);
    }
  }
} 
