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
import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.List;

import VASSAL.tools.opcache.Op;

/**
 * An {@link ImageOp} which uses a fixed image as its source.
 * <code>ImageSourceOp</code> holds a reference to the <code>Image</code>
 * it was constructed with in order to prevent it from being garbage
 * collected from the soft cache, since this op has no way of recreating
 * the source image.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class ImageSourceOpBitmapImpl extends AbstractTiledOpImpl
                                     implements SourceOp {
  private final BufferedImage image;
  private final int hash;

  /**
   * Constructs an <code>ImageOp</code> which will hold and return
   * the image it is constructed with.
   *
   * @param image the source image
   * @throws IllegalArgumentException if <code>image == null</code>.
   */
  public ImageSourceOpBitmapImpl(BufferedImage image) {
    if (image == null) throw new IllegalArgumentException();
    this.image = image;
    hash = image.hashCode();
  }

  public List<Op<?>> getSources() {
    return Collections.emptyList();
  }

  /** {@inheritDoc} */
  public BufferedImage eval() {
    return image;
  }

  /** {@inheritDoc} */
  protected void fixSize() {
    size = new Dimension(image.getWidth(null), image.getHeight(null));
  }

  protected ImageOp createTileOp(int tileX, int tileY) {
    return new SourceTileOpBitmapImpl(this, tileX, tileY);
  }

  public String getName() {
    return null;
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;
    return image.equals(((ImageSourceOpBitmapImpl) o).image);
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
