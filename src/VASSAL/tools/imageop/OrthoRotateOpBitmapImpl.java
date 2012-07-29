/*
 * $Id$
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.builder.HashCodeBuilder;

import VASSAL.tools.image.ImageUtils;

public class OrthoRotateOpBitmapImpl extends AbstractTiledOpImpl
                                     implements RotateOp {
  private final ImageOp sop;
  private final int angle;
  private final int hash;

  public OrthoRotateOpBitmapImpl(ImageOp sop, int angle) {
    if (sop == null) throw new IllegalArgumentException();

    angle = (360 + (angle % 360)) % 360;  // put angle in [0,360)
    if (angle % 90 != 0) throw new IllegalArgumentException();

    // angle is now in { 0, 90, 180, 270 }.

    this.sop = sop;
    this.angle = angle / 90;

    hash = new HashCodeBuilder().append(sop)
                                .append(angle)
                                .toHashCode();
  }

  public List<VASSAL.tools.opcache.Op<?>> getSources() {
    return Collections.<VASSAL.tools.opcache.Op<?>>singletonList(sop);
  }

  public BufferedImage eval() throws Exception {
    final BufferedImage src = sop.getImage(null);
    if (size == null) fixSize();

    // remain opaque if our parent image is
    final BufferedImage dst = ImageUtils.createCompatibleImage(
        size.width, size.height, src.getTransparency() != BufferedImage.OPAQUE
    );

    final Graphics2D g = dst.createGraphics();
    g.rotate(Math.PI/2.0*angle, src.getWidth()/2.0, src.getHeight()/2.0);
    g.drawImage(src, 0, 0, null);
    g.dispose();

    return dst;
  }

  protected void fixSize() {
    if ((size = getSizeFromCache()) == null) {
      size = sop.getSize();

      // transpose dimensions for 90- and 270-degree rotations
      if (angle == 1 || angle == 3)
        size.setSize(size.height, size.width);
    }
  }

  public double getAngle() {
    return angle * 90;
  }

  public RenderingHints getHints() {
//    return ImageUtils.getDefaultHints();
    return null;
  }

  protected ImageOp createTileOp(int tileX, int tileY) {
    return new TileOp(this, tileX, tileY);
  }

  private static class TileOp extends AbstractTileOpImpl {
    private final ImageOp sop;
    private final int angle;
    private final int hash;

    public TileOp(OrthoRotateOpBitmapImpl rop, int tileX, int tileY) {
      if (rop == null) throw new IllegalArgumentException();

      if (tileX < 0 || tileX >= rop.getNumXTiles() ||
          tileY < 0 || tileY >= rop.getNumYTiles())
        throw new IndexOutOfBoundsException();

      this.angle = rop.angle;

      final int sx0, sy0, sx1, sy1;

      switch (angle) {
      case 0:
        sx0 = tileX*rop.tileSize.width;
        sy0 = tileY*rop.tileSize.height;
        sx1 = Math.min((tileX+1)*rop.tileSize.width, rop.size.width);
        sy1 = Math.min((tileY+1)*rop.tileSize.height, rop.size.height);
        break;
      case 1:
        sx0 = tileY*rop.tileSize.height;
        sy0 = tileX*rop.tileSize.width;
        sx1 = Math.min((tileY+1)*rop.tileSize.height, rop.size.height);
        sy1 = Math.min((tileX+1)*rop.tileSize.width, rop.size.width);
        break;
      case 2:
        sx1 = rop.size.width - tileX*rop.tileSize.width;
        sy1 = rop.size.height - tileY*rop.tileSize.height;
        sx0 = rop.size.width -
                Math.min((tileX+1)*rop.tileSize.width, rop.size.width);
        sy0 = rop.size.height -
                Math.min((tileY+1)*rop.tileSize.height, rop.size.height);
        break;
      case 3:
      default:
        sx1 = rop.size.height - tileY*rop.tileSize.height;
        sy1 = rop.size.width - tileX*rop.tileSize.width;
        sx0 = rop.size.height -
                Math.min((tileY+1)*rop.tileSize.height, rop.size.height);
        sy0 = rop.size.width -
                Math.min((tileX+1)*rop.tileSize.width, rop.size.width);
        break;
      }

      size = new Dimension(sx1-sx0, sy1-sy0);

      sop = new CropOpBitmapImpl(rop.sop, sx0, sy0, sx1, sy1);

      hash = new HashCodeBuilder().append(sop)
                                  .append(angle)
                                  .toHashCode();
    }

    public List<VASSAL.tools.opcache.Op<?>> getSources() {
      return Collections.<VASSAL.tools.opcache.Op<?>>singletonList(sop);
    }

    public BufferedImage eval() throws Exception {
      final BufferedImage src = sop.getImage(null);

      // remain opaque if our parent image is
      final BufferedImage dst = ImageUtils.createCompatibleImage(
        size.width, size.height, src.getTransparency() != BufferedImage.OPAQUE
      );

      final Graphics2D g = dst.createGraphics();
      g.rotate(Math.PI/2.0*angle, src.getWidth()/2.0, src.getHeight()/2.0);
      g.drawImage(src, 0, 0, null);
      g.dispose();

      return dst;
    }

    protected void fixSize() { }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || o.getClass() != this.getClass()) return false;

      final TileOp op = (TileOp) o;
      return angle == op.angle &&
             sop.equals(op.sop);
    }

    @Override
    public int hashCode() {
      return hash;
    }
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    final OrthoRotateOpBitmapImpl op = (OrthoRotateOpBitmapImpl) o;
    return angle == op.getAngle() && sop.equals(op.sop);
  }

  @Override
  public int hashCode() {
    return hash;
  }
}
