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
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;

import VASSAL.tools.GeneralFilter;
import VASSAL.tools.HashCode;
import VASSAL.tools.ImageUtils;
import VASSAL.tools.RenderingClues;

/**
 * An {@link ImageOp} which rotates and scales its source. Rotation
 * is about the center of the source image, and scaling ranges from
 * <code>(0,Double.MAX_VALUE)</code>. If a source is to be both rotated
 * and scaled, using one <code>RotateScaleOp</code> will produce better
 * results than doing the rotation and scaling separately with
 * one {@link RotateOp} and one {@link ScaleOp}.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class RotateScaleOp extends AbstractTileOp {
  private final ImageOp sop;
  private final double scale;
  private final double angle;
  private final RenderingHints hints;
  private final int hash;

  /**
   * Constructs an <code>ImageOp</code> which will rotate and scale
   * the image produced by its source <code>ImageOp</code>.
   *
   * @param sop the source operation
   * @param angle the angle of rotation, in degrees
   * @param scale the scale factor
   */
  public RotateScaleOp(ImageOp sop, double angle, double scale) {
    this(sop, angle, scale, RenderingClues.getDefault());
  }
  
  /**
   * Constructs an <code>ImageOp</code> which will rotate and scale
   * the image produced by its source <code>ImageOp</code>.
   *
   * @param sop the source operation
   * @param angle the angle of rotation, in degrees
   * @param scale the scale factor
   * @param hints rendering hints
   * @throws IllegalArgumentException if <code>sop == null</code>.
   */
  public RotateScaleOp(ImageOp sop, double angle, double scale,
                       RenderingHints hints) {
    if (sop == null) throw new IllegalArgumentException();
    if (scale <= 0) throw new IllegalArgumentException();

    this.sop = sop;
    this.angle = angle;
    this.scale = scale;
    this.hints = hints;

    hash = HashCode.hash(scale) ^
           HashCode.hash(angle) ^
           HashCode.hash(hints) ^
           HashCode.hash(sop);
  }

  /**
   * {@inheritDoc}
   *
   * @throws Exception passed up from the source <code>ImageOp</code>.
   */
  protected Image apply() throws Exception {
// FIXME: should not cast to BufferedImage without conversion first!
    return ImageUtils.transform(
      (BufferedImage) sop.getImage(null), scale, angle, hints);
  }

  /** {@inheritDoc} */
  protected void fixSize() {
    if ((size = getSizeFromCache()) == null) {
      size = ImageUtils.transform(
        new Rectangle(sop.getSize()), scale, angle).getSize();
    }
  }

  /**
   * Returns the angle of rotation.
   *
   * @return the angle of rotation, in degrees.
   */
  public double getAngle() {
    return angle;
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
    if (o == null || !(o instanceof RotateScaleOp)) return false;

    final RotateScaleOp op = (RotateScaleOp) o;
    return scale == op.scale && 
           angle == op.angle && 
           hints.equals(op.hints) &&
           sop.equals(op.sop);
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
