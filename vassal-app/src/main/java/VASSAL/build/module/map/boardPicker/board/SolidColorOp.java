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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.AbstractTileOpImpl;
import VASSAL.tools.imageop.AbstractTiledOpImpl;
import VASSAL.tools.imageop.ImageOp;

public class SolidColorOp extends AbstractTiledOpImpl {
  protected final Color color;
  protected final int hash;

  public SolidColorOp(Color color, int w, int h) {
    this.color = color;

    size = new Dimension(w, h);
    tileSize = new Dimension(256, 256);
    numXTiles = (int) Math.ceil((double)size.width / tileSize.width);
    numYTiles = (int) Math.ceil((double)size.height / tileSize.height);
    tiles = null;

    hash = Objects.hash(color, size);
  }

  @Override
  public List<VASSAL.tools.opcache.Op<?>> getSources() {
    return Collections.<VASSAL.tools.opcache.Op<?>>emptyList();
  }

  @Override
  public BufferedImage eval() throws Exception {
    return draw(size.width, size.height, color);
  }

  private static BufferedImage draw(int dw, int dh, Color color) {
    if (dw < 1 || dh < 1) {
      return ImageUtils.NULL_IMAGE;
    }

    // create the destination tile
    final BufferedImage dst = ImageUtils.createCompatibleImage(dw, dh);

    // fill with the color
    final Graphics g = dst.getGraphics();
    g.setColor(color);
    g.fillRect(0, 0, dw, dh);
    g.dispose();
    return dst;
  }

  @Override
  protected void fixTileSize() {
  }

  @Override
  protected void fixSize() {
  }

  public Color getColor() {
    return color;
  }

  @Override
  public ImageOp getTileOp(int tileX, int tileY) {
    return createTileOp(tileX, tileY);
  }

  @Override
  protected ImageOp createTileOp(int tileX, int tileY) {
    return new TileOp(this, tileX, tileY);
  }

  private static class TileOp extends AbstractTileOpImpl {
    private final ImageOp sop;
    private final Color color;
    private final int hash;

    public TileOp(SolidColorOp scop, int tileX, int tileY) {
      if (scop == null) {
        throw new IllegalArgumentException();
      }

      if (tileX < 0 || tileX >= scop.getNumXTiles() ||
          tileY < 0 || tileY >= scop.getNumYTiles()) {
        throw new IndexOutOfBoundsException();
      }

      color = scop.getColor();
      sop = scop;

      size = new Dimension(scop.getTileWidth(), scop.getTileHeight());

      hash = Objects.hash(sop, color, size);
    }

    @Override
    public List<VASSAL.tools.opcache.Op<?>> getSources() {
      return Collections.<VASSAL.tools.opcache.Op<?>>singletonList(sop);
    }

    @Override
    public BufferedImage eval() throws Exception {
      return draw(size.width, size.height, color);
    }

    @Override
    protected void fixSize() {
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || o.getClass() != this.getClass()) return false;

      final TileOp op = (TileOp) o;
      return size.equals(op.size) &&
             color.equals(op.color) &&
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
        ",color=" + color + //NON-NLS
        ",size=" + size + "]"; //NON-NLS
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;
 
    final SolidColorOp op = (SolidColorOp) o;
    return color.equals(op.color) &&
           size.equals(op.size);
  }

  @Override
  public int hashCode() {
    return hash;
  }

  @Override
  public String toString() {
    return getClass().getName() +
      "[color=" + color + //NON-NLS
      ",size=" + size + "]"; //NON-NLS
  }
}
