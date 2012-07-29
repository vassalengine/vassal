/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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

package VASSAL.tools.imageop;

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.builder.HashCodeBuilder;

import VASSAL.tools.image.ImageUtils;

/**
 * An {@link ImageOp} which crops its source.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class CropOpBitmapImpl extends AbstractTiledOpImpl
                              implements CropOp {
  private final ImageOp sop;
  private final int x0, y0, x1, y1;
  private final int hash;

  /**
   * Constructs an <code>ImageOp</code> which will crop the image
   * produced by its source <code>ImageOp</code>.
   *
   * @param sop the source operation
   * @param x0 the x coordinate of the upper-left corner
   * @param y0 the y coordinate of the upper-left corner
   * @param x1 the x coordinate of the lower-right corner
   * @param y1 the y coordinate of the lower-right corner
   */
  public CropOpBitmapImpl(ImageOp sop, int x0, int y0, int x1, int y1) {
    if (sop == null) {
      throw new IllegalArgumentException();
    }

    if (x0 < 0) {
      throw new IllegalArgumentException("left = " + x0);
    }

    if (y0 < 0) {
      throw new IllegalArgumentException("top = " + y0);
    }

    if (x1 <= x0) {
      throw new IllegalArgumentException("left = "+ x0 + ", right = " + x1);
    }

    if (y1 <= y0) {
      throw new IllegalArgumentException("top = " + y0 + ", bottom = " + y1);
    }

    this.sop = sop;
    this.x0 = x0;
    this.y0 = y0;
    this.x1 = x1;
    this.y1 = y1;

    size = new Dimension(x1-x0, y1-y0);

    hash = new HashCodeBuilder().append(sop)
                                .append(x0)
                                .append(y0)
                                .append(x1)
                                .append(y1)
                                .toHashCode();
  }

  public List<VASSAL.tools.opcache.Op<?>> getSources() {
    final Point[] tiles =
      sop.getTileIndices(new Rectangle(x0, y0, x1-x0, y1-y0));

    final ArrayList<VASSAL.tools.opcache.Op<?>> ops =
      new ArrayList<VASSAL.tools.opcache.Op<?>>(tiles.length);

    for (Point tile : tiles) ops.add(sop.getTileOp(tile));

    return ops;
  }

  /**
   * {@inheritDoc}
   *
   * @throws Exception passed up from the source <code>ImageOp</code>.
   * */
  public BufferedImage eval() throws Exception {
    // cobble source from tiles
    final Point[] tiles =
      sop.getTileIndices(new Rectangle(x0, y0, x1-x0, y1-y0));
    final int tw = sop.getTileWidth();
    final int th = sop.getTileHeight();

    // match the transparency of the first tile
    final BufferedImage dst = ImageUtils.createCompatibleImage(
      size.width, size.height,
      sop.getTile(tiles[0], null).getTransparency() != BufferedImage.OPAQUE
    );

    final Graphics2D g = dst.createGraphics();

    for (Point tile : tiles) {
      g.drawImage(sop.getTile(tile, null), tile.x*tw-x0, tile.y*th-y0, null);
    }

    g.dispose();

    return dst;
  }

  protected void fixSize() {}

  protected ImageOp createTileOp(int tileX, int tileY) {
    return new CropOpBitmapImpl(this,
                         tileX*tileSize.width,
                         tileY*tileSize.height,
                         Math.min((tileX+1)*tileSize.width, size.width),
                         Math.min((tileY+1)*tileSize.height, size.height));
  }

  /**
   * Returns the crop rectangle.
   *
   * @return the rectangle to be cropped.
   */
  public Rectangle getRect() {
    return new Rectangle(x0, y0, size.width, size.height);
  }

  public int getX0() {
    return x0;
  }

  public int getY0() {
    return y0;
  }

  public int getX1() {
    return x1;
  }

  public int getY1() {
    return y1;
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    final CropOpBitmapImpl op = (CropOpBitmapImpl) o;
    return x0 == op.getX0() &&
           y0 == op.getY0() &&
           x1 == op.getX1() &&
           y1 == op.getY1() &&
           sop.equals(op.sop);
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
