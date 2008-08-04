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
import java.io.BufferedInputStream;
import java.io.IOException;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;

import VASSAL.build.GameModule;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.SVGRenderer;

/**
 * An {@link ImageOp} which loads an image from the {@link DataArchive}.
 * 
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class SourceOpSVGImpl extends AbstractTiledOpImpl
                             implements SourceOp, SVGOp {
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
  public SourceOpSVGImpl(String name) {
    if (name == null || name.length() == 0)
      throw new IllegalArgumentException();

    this.name = name;
    hash = name.hashCode();
  }

  @Override
  public Image getImage() {
    try {
      return getImage(null);
    }
    catch (CancellationException e) {
      // FIXME: bug until we permit cancellation 
      ErrorDialog.bug(e);
    }
    catch (InterruptedException e) {
      ErrorDialog.bug(e);
    }
    catch (ExecutionException e) {
      OpErrorDialog.error(e, this);
    }

    return null;
  }

  /**
   *  {@inheritDoc}
   *
   * @throws IOException if the image cannot be loaded from the image file.
   */
  public Image apply() throws IOException {
    final DataArchive archive = GameModule.getGameModule().getDataArchive();
    final String path = archive.getImagePrefix() + name;
    final SVGRenderer renderer = new SVGRenderer(
      archive.getArchiveURL() + path,
      new BufferedInputStream(archive.getFileStream(path)));

    return renderer.render();
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
        ReadErrorDialog.error(e, name);
        size = new Dimension();
      }
    }

    tileSize = new Dimension(256,256);

    numXTiles = (int) Math.ceil((double)size.width/tileSize.width);
    numYTiles = (int) Math.ceil((double)size.height/tileSize.height);

    tiles = new ImageOp[numXTiles*numYTiles];
  }

  protected ImageOp createTileOp(int tileX, int tileY) {
    return new SourceTileOpSVGImpl(this, tileX, tileY);
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
    if (o == null || !(o instanceof SourceOp)) return false;
    return name.equals(((SourceOp) o).getName());
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
