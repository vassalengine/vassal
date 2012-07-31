/*
 * $Id$
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
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
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.awt.image.WritableRaster;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.builder.HashCodeBuilder;

import VASSAL.tools.image.GeneralFilter;
import VASSAL.tools.image.ImageUtils;


/**
 * An {@link ImageOp} which scales its source and cobbles scaled tiles
 * from the tile cache.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class ScaleOpTiledBitmapImpl extends ScaleOpBitmapImpl {

  public ScaleOpTiledBitmapImpl(ImageOp sop, double scale) {
    this(sop, scale, defaultHints);
  }

  public ScaleOpTiledBitmapImpl(ImageOp sop, double scale,
                                RenderingHints hints)
  {
    super(sop, scale, hints);
  }

  @Override
  protected ImageOp createTileOp(int tileX, int tileY) {
    final double iscale = 1.0/scale;
    final boolean invPow2 = Math.floor(iscale) == iscale &&
                            (((int) iscale) & (((int) iscale)-1)) == 0;

    if (invPow2 && sop instanceof SourceOp) {
      final String name = ((SourceOp) sop).getName();
      return new SourceOpDiskCacheBitmapImpl(name, tileX, tileY, scale);
    }
    else {
      return new TileOp(this, tileX, tileY);
    }
  }

  private static class TileOp extends AbstractTileOpImpl {
    private final ImageOp rsop;
    private final ImageOp[] sop;
    private final int dx0, dy0, dx1, dy1, dw, dh;
    private final int sx0, sy0, sx1, sy1, sw, sh;
    private final Dimension dd;
    private final Dimension sd;
    private final double scale;
    private final float xscale, yscale;

    private final int tx;
    private final int ty;

    @SuppressWarnings("unused")
    private final RenderingHints hints;

    private final int hash;

    private static final GeneralFilter.Filter downFilter =
      new GeneralFilter.Lanczos3Filter();

    private static final GeneralFilter.Filter upFilter =
      new GeneralFilter.MitchellFilter();

    public TileOp(ScaleOpTiledBitmapImpl rop, int tileX, int tileY) {
      if (rop == null) throw new IllegalArgumentException();

      if (tileX < 0 || tileX >= rop.getNumXTiles() ||
          tileY < 0 || tileY >= rop.getNumYTiles())
        throw new IndexOutOfBoundsException();

      tx = tileX;
      ty = tileY;

      scale = rop.getScale();
      hints = rop.getHints();

      // destination sizes and coordinates
      dd = rop.getSize();

      dx0 = tileX * rop.getTileWidth();
      dy0 = tileY * rop.getTileHeight();
      dw = Math.min(rop.getTileWidth(), dd.width - dx0);
      dh = Math.min(rop.getTileHeight(), dd.height - dy0);

      dx1 = dx0 + dw - 1;
      dy1 = dy0 + dh - 1;

      size = new Dimension(dw, dh);

      if (scale >= 1.0) {
        // we are upscaling
        rsop = rop.sop;
      }
      else {
        // cobble this tile from pyramid tiles at the lub size
        final double nscale =
          1.0/(1 << (int) Math.floor(Math.log(1.0/scale)/Math.log(2)));

        rsop = new ScaleOpTiledBitmapImpl(rop.sop, nscale);
      }

      sd = rsop.getSize();

      // We want dx0 * xscale = sx0, unless that makes xscale = 0.
      xscale = sd.width == 1 ?  dd.width : (float)(dd.width-1)/(sd.width-1);

      // We want dy0 * yscale = sy0, unless that makes yscale = 0.
      yscale = sd.height == 1 ? dd.height : (float)(dd.height-1)/(sd.height-1);

      final float fw = scale < 1.0f ?
        downFilter.getSamplingRadius() : upFilter.getSamplingRadius();

      sx0 = Math.max(0, (int) Math.floor((dx0-fw)/xscale));
      sy0 = Math.max(0, (int) Math.floor((dy0-fw)/yscale));
      sx1 = Math.min(sd.width-1, (int) Math.ceil((dx1+fw)/xscale));
      sy1 = Math.min(sd.height-1, (int) Math.ceil((dy1+fw)/yscale));
      sw = sx1 - sx0 + 1;
      sh = sy1 - sy0 + 1;

      final Rectangle sr = new Rectangle(sx0, sy0, sw, sh);
      final Point[] stiles = rsop.getTileIndices(sr);

      sop = new ImageOp[stiles.length];
      for (int i = 0; i < stiles.length; ++i) {
        sop[i] = rsop.getTileOp(stiles[i]);
      }

      hash = new HashCodeBuilder().append(sop)
                                  .append(dx0)
                                  .append(dy0)
                                  .append(dw)
                                  .append(dh)
                                  .append(tx)
                                  .append(ty)
                                  .toHashCode();
    }

    public List<VASSAL.tools.opcache.Op<?>> getSources() {
      return Arrays.<VASSAL.tools.opcache.Op<?>>asList(sop);
    }

    public BufferedImage eval() throws Exception {
      if (dw < 1 || dh < 1) return ImageUtils.NULL_IMAGE;

      // cobble
      final Point[] tiles =
        rsop.getTileIndices(new Rectangle(sx0, sy0, sw, sh));
      final int tw = rsop.getTileWidth();
      final int th = rsop.getTileHeight();

      BufferedImage src;
      final boolean src_trans =
        ImageUtils.isTransparent(rsop.getTile(tiles[0], null));

      // make sure we have an int-type image
      // and match the transparency of the first tile
      switch (ImageUtils.getCompatibleImageType(rsop.getTile(tiles[0], null))) {
      case BufferedImage.TYPE_INT_RGB:
      case BufferedImage.TYPE_INT_ARGB:
      case BufferedImage.TYPE_INT_ARGB_PRE:
      case BufferedImage.TYPE_INT_BGR:
        src = ImageUtils.createCompatibleImage(sw, sh, src_trans);
        break;
      default:
        src = new BufferedImage(
          sw, sh, src_trans ?
            BufferedImage.TYPE_INT_ARGB_PRE : BufferedImage.TYPE_INT_RGB
        );
      }

      final Graphics2D g = src.createGraphics();

      for (Point tile : tiles) {
        g.drawImage(rsop.getTile(tile, null),
                    tile.x*tw-sx0, tile.y*th-sy0, null);
      }

      g.dispose();

      final int src_data[] =
        ((DataBufferInt) src.getRaster().getDataBuffer()).getData();

      final WritableRaster dstR = src.getColorModel()
                                     .createCompatibleWritableRaster(dw, dh);
      final int dst_data[] = ((DataBufferInt) dstR.getDataBuffer()).getData();

      final int src_type;
      if (!src_trans) {
        src_type = GeneralFilter.OPAQUE;
      }
      else if (src.isAlphaPremultiplied()) {
        src_type = GeneralFilter.TRANS_PREMULT;
      }
      else {
        src_type = GeneralFilter.TRANS_UNPREMULT;
      }

      GeneralFilter.resample(
        src_data, true, sx0, sy0, sx1, sy1, sw, sh, src_type, sd.width, sd.height,
//        src_data, sx0, sy0, sx1, sy1, sw, sh, src_type, sd.width, sd.height,
        dst_data, dx0, dy0, dx1, dy1, dw, dh, dd.width, dd.height,
        xscale, yscale, scale < 1.0f ? downFilter : upFilter
      );

      return ImageUtils.toCompatibleImage(new BufferedImage(
        src.getColorModel(),
        dstR,
        src.isAlphaPremultiplied(),
        null
      ));
    }

    protected void fixSize() { }

    /** {@inheritDoc} */
    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || o.getClass() != this.getClass()) return false;

      final TileOp op = (TileOp) o;
      return dx0 == op.dx0 &&
             dy0 == op.dy0 &&
             dw == op.dw &&
             dh == op.dh &&
             tx == op.tx &&
             ty == op.ty &&
             scale == op.scale &&
             Arrays.equals(sop, op.sop);
    }

    /** {@inheritDoc} */
    @Override
    public int hashCode() {
      return hash;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
      return getClass().getName() +
        "[sop=" + Arrays.toString(sop) + ",scale=" + scale +
        ",dx0=" + dx0 + ",dy0=" + dy0 + ",dw=" + dw + ",dy=" + dh + "]";
    }
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    final ScaleOpTiledBitmapImpl op = (ScaleOpTiledBitmapImpl) o;
    return scale == op.scale &&
           sop.equals(op.sop) &&
           hints.equals(op.hints);
  }
}
