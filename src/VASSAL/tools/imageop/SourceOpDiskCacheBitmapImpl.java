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
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.builder.HashCodeBuilder;

import VASSAL.build.GameModule;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageTileSource;

/**
 * An {@link ImageOp} which loads tiles from the tile cache.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class SourceOpDiskCacheBitmapImpl extends AbstractTileOpImpl
                                         implements SourceOp {

  /** The name of the image file. */
  protected final String name;

  /** The cached hash code of this object. */
  protected final int hash;

  protected final int tileX;
  protected final int tileY;
  protected double scale;

  protected final ImageTileSource tileSrc;

  /**
   * Constructs an <code>ImageOp</code> which will load the given file.
   *
   * @param name the name of the image to load
   * @throws IllegalArgumentException
   *    if <code>name</code> is <code>null</code>.
   */
  public SourceOpDiskCacheBitmapImpl(String name,
                                     int tileX, int tileY, double scale) {
    this(name, tileX, tileY, scale,
         GameModule.getGameModule().getImageTileSource());
  }

  public SourceOpDiskCacheBitmapImpl(
    String name,
    int tileX,
    int tileY,
    double scale,
    ImageTileSource tileSrc)
  {
    if (name == null) throw new IllegalArgumentException();
    if (name.length() == 0) throw new IllegalArgumentException();
    if (tileX < 0) throw new IllegalArgumentException();
    if (tileY < 0) throw new IllegalArgumentException();
    if (scale <= 0) throw new IllegalArgumentException();
    if (tileSrc == null) throw new IllegalArgumentException();

    this.name = name;
    this.tileX = tileX;
    this.tileY = tileY;
    this.scale = scale;
    this.tileSrc = tileSrc;

    hash = new HashCodeBuilder().append(name)
                                .append(tileX)
                                .append(tileY)
                                .append(scale)
                                .toHashCode();
  }

  public List<VASSAL.tools.opcache.Op<?>> getSources() {
    return Collections.emptyList();
  }

  /**
   * {@inheritDoc}
   *
   * @throws IOException if the image cannot be loaded from the image file.
   */
  public BufferedImage eval() throws ImageIOException {
    return tileSrc.getTile(name, tileX, tileY, scale);
  }

  /** {@inheritDoc} */
  protected void fixSize() {
    if ((size = getSizeFromCache()) == null) {
      size = getImageSize();
    }
  }

  protected Dimension getImageSize() {
    try {
      return tileSrc.getTileSize(name, tileX, tileY, scale);
    }
    catch (ImageIOException e) {
      if (!Op.handleException(e)) ErrorDialog.bug(e);
    }

    return new Dimension();
  }

  /**
   * Returns the name of the image which {@link #getImage} will produce.
   *
   * @return the name of the image in the {@link DataArchive}.
   */
  public String getName() {
    return name;
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    final SourceOpDiskCacheBitmapImpl s = (SourceOpDiskCacheBitmapImpl) o;
    return name.equals(s.name) &&
           tileX == s.tileX &&
           tileY == s.tileY &&
           scale == s.scale;
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }

  /** {@inheritDoc} */
  @Override
  public String toString() {
    return getClass().getName() + "[name=" + name +
      ",tileX=" + tileX + ",tileY=" + tileY + ",scale=" + scale + "]";
  }
}
