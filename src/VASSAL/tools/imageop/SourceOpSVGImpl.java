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
import java.io.BufferedInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;

import VASSAL.build.GameModule;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageNotFoundException;
import VASSAL.tools.image.svg.SVGImageUtils;
import VASSAL.tools.image.svg.SVGRenderer;
import VASSAL.tools.io.IOUtils;

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

  /** The zip file from which the image will be loaded */
  protected final DataArchive archive;

  /**
   * Constructs an <code>ImageOp</code> which will load the given file.
   *
   * @param name the name of the image to load
   * @throws IllegalArgumentException
   *    if <code>name</code> is <code>null</code>.
   */
  public SourceOpSVGImpl(String name) {
    this(name, GameModule.getGameModule().getDataArchive());
  }

  public SourceOpSVGImpl(String name, DataArchive archive) {
    if (name == null || name.length() == 0 || archive == null)
      throw new IllegalArgumentException();

    this.name = name;
    this.archive = archive;
    hash = name.hashCode() ^ archive.hashCode();
  }

  public List<VASSAL.tools.opcache.Op<?>> getSources() {
    return Collections.emptyList();
  }

  /**
   *  {@inheritDoc}
   *
   * @throws IOException if the image cannot be loaded from the image file.
   */
  public BufferedImage eval() throws ImageIOException {
    try {
      final SVGRenderer renderer = new SVGRenderer(
        archive.getURL(name),
        new BufferedInputStream(archive.getInputStream(name))
      );

      return renderer.render();
    }
    catch (FileNotFoundException e) {
      throw new ImageNotFoundException(name, e);
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
  }

  // FIXME: we need a way to invalidate ImageOps when an exception is thrown?
  // Maybe size should go to -1,-1 when invalid?

  /** {@inheritDoc} */
  protected void fixSize() {
    if ((size = getSizeFromCache()) == null) {
      size = getImageSize();
    }
  }

  protected Dimension getImageSize() {
    try {
      InputStream in = null;
      try {
        in = archive.getInputStream(name);

        final Dimension d = SVGImageUtils.getImageSize(name, in);
        in.close();
        return d;
      }
      catch (ImageIOException e) {
        // Don't wrap, just rethrow.
        throw e;
      }
      catch (FileNotFoundException e) {
        throw new ImageNotFoundException(name, e);
      }
      catch (IOException e) {
        throw new ImageIOException(name, e);
      }
      finally {
        IOUtils.closeQuietly(in);
      }
    }
    catch (IOException e) {
      if (!Op.handleException(e)) ErrorDialog.bug(e);
    }

    return new Dimension();
  }

  protected ImageOp createTileOp(int tileX, int tileY) {
    return new SourceTileOpSVGImpl(this, tileX, tileY);
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

    final SourceOpSVGImpl s = (SourceOpSVGImpl) o;
    return archive == s.archive && name.equals(s.name);
  }

  /** {@inheritDoc} */
  @Override
  public int hashCode() {
    return hash;
  }
}
