/*
 * $Id: SourceOpBitmapImpl.java 6010 2009-09-05 21:16:56Z uckelman $
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

import java.io.File;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.tools.DataArchive;

/**
 * An {@link ImageOp} which loads an image from the {@link DataArchive},
 * but produces tiles loaded from the tile cache.
 *
 * @since 3.2.0
 * @author Joel Uckelman 
 */
public class SourceOpTiledBitmapImpl extends SourceOpBitmapImpl {

  /**
   * Constructs an <code>ImageOp</code> which will load the given file.
   *
   * @param name the name of the image to load
   * @throws IllegalArgumentException
   *    if <code>name</code> is <code>null</code>.
   */
  public SourceOpTiledBitmapImpl(String name) {
    this(name, GameModule.getGameModule().getDataArchive());
  } 

  public SourceOpTiledBitmapImpl(String name, DataArchive archive) {
    super(name, archive);
  }

  @Override
  protected ImageOp createTileOp(int tileX, int tileY) {
    return new SourceOpDiskCacheBitmapImpl(name, tileX, tileY, 1.0);
  }

  /** {@inheritDoc} */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    final SourceOpTiledBitmapImpl s = (SourceOpTiledBitmapImpl) o;
    return archive == s.archive && name.equals(s.name);
  }
}
