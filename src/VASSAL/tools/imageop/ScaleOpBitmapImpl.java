/*
 * $Id: ScaleOp.java 2734 2007-12-21 14:30:36Z uckelman $
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
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;

import VASSAL.tools.GeneralFilter;
import VASSAL.tools.HashCode;
import VASSAL.tools.ImageUtils;

/**
 * An {@link ImageOp} which scales its source.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class ScaleOpBitmapImpl extends AbstractTiledOpImpl
                               implements ScaleOp {
  private final ImageOp sop;
  private final double scale;
  private final RenderingHints hints;
  private final int hash;

  /**
   * Constructs an <code>ImageOp</code> which will scale
   * the image produced by its source <code>ImageOp</code>.
   *
   * @param sop the source operation
   * @param scale the scale factor
   */
  public ScaleOpBitmapImpl(ImageOp sop, double scale) {
    this(sop, scale, ImageUtils.getDefaultHints());
  }

  /**
   * Constructs an <code>ImageOp</code> which will scale
   * the image produced by its source <code>ImageOp</code>.
   *
   * @param sop the source operation
   * @param scale the scale factor
   * @param hints rendering hints
   */
  public ScaleOpBitmapImpl(ImageOp sop, double scale, RenderingHints hints) {
    if (sop == null) throw new IllegalArgumentException();
    if (scale <= 0) throw new IllegalArgumentException();

    this.sop = sop;
    this.scale = scale;
    this.hints = hints;

    hash = HashCode.hash(scale) ^
           HashCode.hash(hints) ^
           HashCode.hash(sop);
  }

  /**
   * {@inheritDoc}
   *
   * @throws Exception passed up from the source <code>ImageOp</code>.
   */
  public Image apply() throws Exception {
// FIXME: should not cast to BufferedImage without conversion first!
    return ImageUtils.transform(
      (BufferedImage) sop.getImage(null), scale, 0.0, hints);
  }

  /** {@inheritDoc} */
  protected void fixSize() {
    if ((size = getSizeFromCache()) == null) {
      size = ImageUtils.transform(
        new Rectangle(sop.getSize()), scale, 0.0).getSize();
    }

    tileSize = new Dimension(256,256);

    numXTiles = (int) Math.ceil((double)size.width/tileSize.width);
    numYTiles = (int) Math.ceil((double)size.height/tileSize.height);

    tiles = new ImageOp[numXTiles*numYTiles];
  }

  protected ImageOp getTileOp(int tileX, int tileY) {
    ImageOp top = tiles[tileY*numXTiles + tileX];
    if (top == null) {
      top = tiles[tileY*numXTiles + tileX] =
        new TileOp(this, tileX, tileY);
    }
  
    return top;
  }

  private static class TileOp extends AbstractTileOpImpl {
     private final ImageOp sop;
     private final int dx0, dy0, dw, dh;
     private final double scale;
     private final RenderingHints hints;
     private final int hash;
 
     private static final GeneralFilter.Filter downFilter =
       new GeneralFilter.Lanczos3Filter();
     private static final GeneralFilter.Filter upFilter =
       new GeneralFilter.MitchellFilter();
 
     public TileOp(ScaleOp rop, int tileX, int tileY) {
       if (rop == null) throw new IllegalArgumentException();
 
       if (tileX < 0 || tileX >= rop.getNumXTiles() ||
           tileY < 0 || tileY >= rop.getNumYTiles())
         throw new IndexOutOfBoundsException();
 
      this.sop = rop.getSource();
 
      scale = rop.getScale();
      hints = rop.getHints();
 
      final Rectangle sr =
         new Rectangle(0, 0,
                       (int)(sop.getWidth()*scale),
                       (int)(sop.getHeight()*scale));
 
      dx0 = tileX*rop.getTileWidth();
      dy0 = tileY*rop.getTileHeight();
      dw = Math.min(rop.getTileWidth(), sr.width - dx0);
      dh = Math.min(rop.getTileHeight(), sr.height - dy0);
 
      size = new Dimension(dw,dh);
 
      final int PRIME = 31;
      int result = 1;
      result = PRIME * result + HashCode.hash(sop);
      result = PRIME * result + HashCode.hash(dx0);
      result = PRIME * result + HashCode.hash(dy0);
      result = PRIME * result + HashCode.hash(dw);
      result = PRIME * result + HashCode.hash(dh);
      hash = result;
    }

    public Image apply() throws Exception {
      BufferedImage src = (BufferedImage) sop.getImage(null);
 
      final Rectangle sr =
        new Rectangle(0, 0,
                      (int)(sop.getWidth()*scale),
                      (int)(sop.getHeight()*scale));
 
      final WritableRaster dstR = src.getColorModel()
                                     .createCompatibleWritableRaster(dw, dh)
                                     .createWritableTranslatedChild(dx0, dy0);
      // zoom! zoom!
      GeneralFilter.zoom(dstR, sr, src, scale < 1.0f ? downFilter : upFilter);
/*
      final BufferedImage si = new BufferedImage(src.getColorModel(),
      dstR.createWritableTranslatedChild(0,0), false, null);
      return angle == 0.0 ? si : ImageUtils.transform(si, 1.0, angle, hints);
*/
      return new BufferedImage(src.getColorModel(),
        dstR.createWritableTranslatedChild(0,0), false, null);
    }
 
    protected void fixSize() { }
 
    public ImageOp getSource() {
      return sop;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || !(o instanceof TileOp)) return false;

      final TileOp op = (TileOp) o;
      return dx0 == op.dx0 &&
             dy0 == op.dy0 &&
             dw == op.dw &&
             dh == op.dh &&
             scale == op.scale &&
             sop.equals(op.sop);
    }
 
    @Override
    public int hashCode() {
      return hash;
    }
  }

  public ImageOp getSource() {
    return sop;
  }

  public RenderingHints getHints() {
    return hints;
  }

  /**
   * Returns the scale factor.
   *
   * @return the scale factor, in the range <code>(0,Double.MAX_VALUE]</code>.
   */
  public double getScale() {
    return scale;
  }
  
  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || !(o instanceof ScaleOp)) return false;

    final ScaleOp op = (ScaleOp) o;
    return scale == op.getScale() && 
           hints.equals(op.getHints()) &&
           sop.equals(op.getSource());
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
