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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;

import VASSAL.build.GameModule;
//import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ImageUtils;

/**
 * An {@link ImageOp} which loads an image from the {@link DataArchive}.
 * 
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class SourceOp extends AbstractTiledOp {
  /** The name of the image file. */
  protected final String name;

  /** The cached hash code of this object. */
  protected final int hash;

  /**
   * Constructs an <code>ImageOp</code> which will load the given file.
   *
   * @param name the name of the image to load
   * @throws IllegalArgumentException
   *    if <code>name</code> is <code>null</code>.
   */
  public SourceOp(String name) {
    if (name == null || name.isEmpty())
      throw new IllegalArgumentException();
    this.name = name;
    hash = name.hashCode();
  }

  /**
   *  {@inheritDoc}
   *
   * @throws IOException if the image cannot be loaded from the image file.
   */
  protected Image apply() throws IOException {
    return GameModule.getGameModule().getDataArchive().getImage(name);
  }

// FIXME: we need a way to invalidate ImageOps when an exception is thrown?
// Maybe size should go to -1,-1 when invalid?

  /** {@inheritDoc} */
  protected void fixSize() {
    if ((size = getSizeFromCache()) == null) {
      try {
        size = GameModule.getGameModule().getDataArchive().getImageSize(name);
      }
      catch (IOException e) {
        e.printStackTrace();
        size = new Dimension(-1,-1);
      }
    }

    tileSize = new Dimension(256,256);

    numXTiles = (int) Math.ceil((double)size.width/tileSize.width);
    numYTiles = (int) Math.ceil((double)size.height/tileSize.height);

    tiles = new ImageOp[numXTiles*numYTiles];
  }

  /** {@inheritDoc} */
  public ImageOp getTileOp(int tileX, int tileY) {
    ImageOp top = tiles[tileY*numXTiles + tileX];
    if (top == null) {
      top = tiles[tileY*numXTiles + tileX]
          = new SourceTileOp(this, tileX, tileY);
    }

    return top;
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
    if (o == null || !(o instanceof SourceOp)) return false;
    return name.equals(((SourceOp) o).name);
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
