/*
 * Copyright (c) 2020 by Joel Uckelman
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
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;

import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.AbstractTileOpImpl;
import VASSAL.tools.imageop.AbstractTiledOpImpl;
import VASSAL.tools.imageop.ImageOp;

public class GridOp extends AbstractTiledOpImpl {
  protected final ImageOp sop;
  protected final MapGrid grid;
  protected final double scale;
  protected final boolean reversed;
  protected final RenderingHints hints;

  protected final int hash;

  public GridOp(ImageOp sop, MapGrid grid, double scale, boolean reversed, RenderingHints hints) {
    this.sop = sop;
    this.grid = grid;
    this.scale = scale;
    this.reversed = reversed;
    this.hints = hints;

    size = sop.getSize();
    hash = Objects.hash(sop, grid, scale, reversed, hints);
  }

  public MapGrid getGrid() {
    return grid;
  }

  public double getScale() {
    return scale;
  }

  public boolean getReversed() {
    return reversed;
  }

  public RenderingHints getHints() {
    return hints;
  }

  @Override
  public List<VASSAL.tools.opcache.Op<?>> getSources() {
    return Collections.<VASSAL.tools.opcache.Op<?>>singletonList(sop);
  }

  @Override
  public BufferedImage eval() throws Exception {
    final Rectangle bounds = new Rectangle(0, 0, size.width, size.height);
    return draw(0, 0, size.width, size.height, sop, grid, bounds, scale, reversed, hints);
  }

  private static BufferedImage draw(int dx0, int dy0, int dw, int dh, ImageOp sop, MapGrid grid, Rectangle bounds, double scale, boolean reversed, RenderingHints hints) throws ExecutionException, InterruptedException {
    if (dw < 1 || dh < 1) {
      return ImageUtils.NULL_IMAGE;
    }

    // get the source tile and create the destination tile
    final BufferedImage src = sop.getImage(null);
    final BufferedImage dst = ImageUtils.createCompatibleImage(
      dw, dh,
      src.getTransparency() != BufferedImage.OPAQUE
    );

    // paint the source onto the destination
    final Graphics2D g = (Graphics2D) dst.getGraphics();
    g.setRenderingHints(hints);
    g.drawImage(src, 0, 0, null);

    // paint the grid onto the destination
    final Rectangle visible = new Rectangle(dx0, dy0, dw, dh);
    g.translate(-dx0, -dy0);
    g.setClip(visible);
    grid.draw(g, bounds, visible, scale, reversed);

    g.dispose();
    return dst;
  }

  @Override
  protected void fixSize() {
  }

  @Override
  protected ImageOp createTileOp(int tileX, int tileY) {
    return new TileOp(this, tileX, tileY);
  }

  private static class TileOp extends AbstractTileOpImpl {
    private final ImageOp sop;
    private final Rectangle bounds;
    private final int dx0, dy0, dw, dh;
    private final MapGrid grid;
    private final double scale;
    private final boolean reversed;
    private final RenderingHints hints;

    private final int hash;

    public TileOp(GridOp gop, int tileX, int tileY) {
      if (gop == null) {
        throw new IllegalArgumentException();
      }

      if (tileX < 0 || tileX >= gop.getNumXTiles() ||
          tileY < 0 || tileY >= gop.getNumYTiles()) {
        throw new IndexOutOfBoundsException();
      }

      scale = gop.getScale();
      grid = gop.getGrid();
      reversed = gop.getReversed();
      hints = gop.getHints();
      bounds = new Rectangle(0, 0, gop.getWidth(), gop.getHeight());

      final ImageOp sgop = (ImageOp) gop.getSources().get(0);

      final int stw = sgop.getTileWidth();
      final int sth = sgop.getTileHeight();

      dx0 = tileX * stw;
      dy0 = tileY * sth;
      dw = Math.min(stw, sgop.getWidth() - dx0);
      dh = Math.min(sth, sgop.getHeight() - dy0);

      size = new Dimension(dw, dh);

      sop = sgop.getTileOp(tileX, tileY);

      hash = Objects.hash(sop, dx0, dy0, dw, dh, grid, scale, reversed, hints);
    }

    @Override
    public List<VASSAL.tools.opcache.Op<?>> getSources() {
      return Collections.<VASSAL.tools.opcache.Op<?>>singletonList(sop);
    }

    @Override
    public BufferedImage eval() throws Exception {
      return draw(dx0, dy0, dw, dh, sop, grid, bounds, scale, reversed, hints);
    }

    @Override
    protected void fixSize() {
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || o.getClass() != this.getClass()) return false;

      final TileOp op = (TileOp) o;
      return dx0 == op.dx0 &&
             dy0 == op.dy0 &&
             dw == op.dw &&
             dh == op.dh &&
             scale == op.scale &&
             reversed == op.reversed &&
             grid.equals(op.grid) &&
             hints.equals(op.hints) &&
             sop.equals(op.sop);
    }

    @Override
    public int hashCode() {
      return hash;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
      return getClass().getName() +
        "[sop=" + sop + //NON-NLS
        ",scale=" + scale + //NON-NLS
        ",dx0=" + dx0 + //NON-NLS
        ",dy0=" + dy0 + //NON-NLS
        ",dw=" + dw + //NON-NLS
        ",dy=" + dh + //NON-NLS
        ",scale= " + scale + //NON-NLS
        ",reversed=" + reversed + //NON-NLS
        ",hints=" + hints + "]"; //NON-NLS
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    final GridOp op = (GridOp) o;
    return reversed == op.reversed &&
           scale == op.scale &&
           sop.equals(op.sop) &&
           grid.equals(op.grid) &&
           hints.equals(op.hints);
  }

  @Override
  public int hashCode() {
    return hash;
  }

  @Override
  public String toString() {
    return getClass().getName() +
      "[sop=" + sop + //NON-NLS
      ",grid=" + grid + //NON-NLS
      ",scale=" + scale + //NON-NLS
      ",reversed=" + reversed + //NON-NLS
      ",hints=" + hints + "]"; //NON-NLS
  }
}
