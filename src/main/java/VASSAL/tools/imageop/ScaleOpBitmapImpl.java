/*
 *
 * Copyright (c) 2007-2010 by Joel Uckelman
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
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import VASSAL.tools.image.GeneralFilter;
import VASSAL.tools.image.ImageUtils;

/**
 * An {@link ImageOp} which scales its source.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class ScaleOpBitmapImpl extends AbstractTiledOpImpl
                               implements ScaleOp {
  protected final ImageOp sop;
  protected final double scale;
  protected final RenderingHints hints;
  protected final int hash;

  // FIXME: We try to always use the same hints object because hints is
  // used in our equals() and RenderingHints.equals() is ridiculously slow
  // if a full comparison is made. This way hints == defaultHints, usually,
  // and so a quick equality comparison succeeds.
  protected static final RenderingHints defaultHints =
    ImageUtils.getDefaultHints();

  /**
   * Constructs an <code>ImageOp</code> which will scale
   * the image produced by its source <code>ImageOp</code>.
   *
   * @param sop the source operation
   * @param scale the scale factor
   */
  public ScaleOpBitmapImpl(ImageOp sop, double scale) {
    this(sop, scale, defaultHints);
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
    if (sop == null)
      throw new IllegalArgumentException("Attempt to scale null image");
    if (scale <= 0)
      throw new IllegalArgumentException("Cannot scale image at " + scale);

    this.sop = sop;
    this.scale = scale;
    this.hints = hints;

    hash = new HashCodeBuilder().append(sop)
                                .append(scale)
                                .append(hints)
                                .toHashCode();
  }

  @Override
  public List<VASSAL.tools.opcache.Op<?>> getSources() {
    return Collections.<VASSAL.tools.opcache.Op<?>>singletonList(sop);
  }

  /**
   * {@inheritDoc}
   *
   * @throws Exception passed up from the source <code>ImageOp</code>.
   */
  @Override
  public BufferedImage eval() throws Exception {
    return ImageUtils.transform(sop.getImage(null), scale, 0.0, hints);
  }

  /** {@inheritDoc} */
  @Override
  protected void fixSize() {
    if ((size = getSizeFromCache()) == null) {
      size = ImageUtils.transform(
        new Rectangle(sop.getSize()), scale, 0.0).getSize();
    }
  }

  @Override
  protected ImageOp createTileOp(int tileX, int tileY) {
    return new TileOp(this, tileX, tileY);
  }

  private static class TileOp extends AbstractTileOpImpl {
    private final ImageOp sop;
    private final int dx0, dy0, dw, dh;
    private final double scale;
    @SuppressWarnings("unused")
    private final RenderingHints hints;
    private final int hash;

    private static final GeneralFilter.Filter downFilter =
      new GeneralFilter.Lanczos3Filter();
    private static final GeneralFilter.Filter upFilter =
      new GeneralFilter.MitchellFilter();

    public TileOp(ScaleOpBitmapImpl rop, int tileX, int tileY) {
      if (rop == null) throw new IllegalArgumentException();

      if (tileX < 0 || tileX >= rop.getNumXTiles() ||
        tileY < 0 || tileY >= rop.getNumYTiles())
        throw new IndexOutOfBoundsException();

      sop = rop.sop;

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

      hash = new HashCodeBuilder().append(sop)
                                  .append(dx0)
                                  .append(dy0)
                                  .append(dw)
                                  .append(dh)
                                  .toHashCode();
    }

    @Override
    public List<VASSAL.tools.opcache.Op<?>> getSources() {
      return Collections.<VASSAL.tools.opcache.Op<?>>singletonList(sop);
    }

    @Override
    public BufferedImage eval() throws Exception {
      if (dw < 1 || dh < 1) return ImageUtils.NULL_IMAGE;

      // ensure that src is a type which GeneralFilter can handle
      final BufferedImage src = ImageUtils.coerceToIntType(sop.getImage(null));

      final Rectangle sr =
        new Rectangle(0, 0,
          (int)(sop.getWidth()*scale),
          (int)(sop.getHeight()*scale));

      final WritableRaster dstR = src.getColorModel()
                                     .createCompatibleWritableRaster(dw, dh)
                                     .createWritableTranslatedChild(dx0, dy0);
      // zoom! zoom!
      GeneralFilter.zoom(dstR, sr, src, scale < 1.0f ? downFilter : upFilter);

      return ImageUtils.toCompatibleImage(new BufferedImage(
        src.getColorModel(),
        dstR.createWritableTranslatedChild(0,0),
        src.isAlphaPremultiplied(),
        null
      ));
    }

    @Override
    protected void fixSize() { }

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
        "[sop=" + sop + ",scale=" + scale +
        ",dx0=" + dx0 + ",dy0=" + dy0 + ",dw=" + dw + ",dy=" + dh + "]";
    }
  }

  @Override
  public RenderingHints getHints() {
    return hints;
  }

  /**
   * Returns the scale factor.
   *
   * @return the scale factor, in the range <code>(0,Double.MAX_VALUE]</code>.
   */
  @Override
  public double getScale() {
    return scale;
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    final ScaleOpBitmapImpl op = (ScaleOpBitmapImpl) o;
    return scale == op.scale &&
           sop.equals(op.sop) &&
           hints.equals(op.hints);
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
      "[sop=" + sop + ",scale=" + scale + ",hints=" + hints + "]";
  }
}
