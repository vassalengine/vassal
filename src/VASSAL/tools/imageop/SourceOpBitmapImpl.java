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
import java.io.IOException;
import java.io.InputStream;

import VASSAL.build.GameModule;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.ImageUtils;

/**
 * An {@link ImageOp} which loads an image from the {@link DataArchive}.
 * 
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class SourceOpBitmapImpl extends AbstractTiledOpImpl
                                implements SourceOp {
  /** The name of the image file. */
  protected final String name;

  /** The cached hash code of this object. */
  protected final int hash;
  
  /** The zip file from which the image will be loaded */
  protected DataArchive archive;

  /**
   * Constructs an <code>ImageOp</code> which will load the given file.
   *
   * @param name the name of the image to load
   * @throws IllegalArgumentException
   *    if <code>name</code> is <code>null</code>.
   */
  public SourceOpBitmapImpl(String name) {
    this(name,GameModule.getGameModule().getDataArchive());
  }
  
  public SourceOpBitmapImpl(String name, DataArchive archive) {
    if (name == null || name.length() == 0)
      throw new IllegalArgumentException();
    this.name = name;
    this.archive = archive;
    hash = name.hashCode();
  }

  /**
   *  {@inheritDoc}
   *
   * @throws IOException if the image cannot be loaded from the image file.
   */
  public Image apply() throws IOException {
    if (size == null) fixSize();

    if (ImageUtils.isLargeImage(size.width, size.height)) {
      return ImageUtils.getLargeImage(getImageStream());
    }
    else {
      return ImageUtils.getSmallImage(getImageStream());
    }
  }

  protected InputStream getImageStream() throws IOException {
    return archive.getImageInputStream(name);
  }

// FIXME: we need a way to invalidate ImageOps when an exception is thrown?
// Maybe size should go to -1,-1 when invalid?

  /** {@inheritDoc} */
  protected void fixSize() {
    if ((size = getSizeFromCache()) == null) {
        size = getImageSize();
    }

    tileSize = new Dimension(256,256);

    numXTiles = (int) Math.ceil((double)size.width/tileSize.width);
    numYTiles = (int) Math.ceil((double)size.height/tileSize.height);

    tiles = new ImageOp[numXTiles*numYTiles];
  }

  protected Dimension getImageSize() {
    try {
      return archive.getImageSize(name);
    }
    catch (IOException e) {
      ErrorLog.warn(e);
      return new Dimension(-1,-1);
    }
  }

  protected ImageOp createTileOp(int tileX, int tileY) {
    return new SourceTileOpBitmapImpl(this, tileX, tileY);
  }

  public ImageOp getSource() {
    return null;
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
    if (o == null || !(o instanceof SourceOpBitmapImpl)) return false;
    if (!name.equals(((SourceOpBitmapImpl) o).getName())) return false;
    return archive == null ? ((SourceOpBitmapImpl)o).archive == null : archive.getName().equals(((SourceOpBitmapImpl)o).archive.getName());
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
