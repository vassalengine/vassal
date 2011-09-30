/*
 * $Id$
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.builder.HashCodeBuilder;

import VASSAL.build.GameModule;
import VASSAL.tools.DataArchive;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageNotFoundException;
import VASSAL.tools.image.svg.SVGRenderer;
import VASSAL.tools.opcache.Op;

/**
 * An {@link ImageOp} for producing tiles directly from a source,
 * without cobbling tiles from the source.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class SourceTileOpSVGImpl extends AbstractTileOpImpl
                                 implements SVGOp {
  private final SVGOp sop;
  private final int x0, y0, x1, y1;
  private final int hash;

  public SourceTileOpSVGImpl(SVGOp sop, int tileX, int tileY) {
    if (sop == null) throw new IllegalArgumentException();

    if (tileX < 0 || tileX >= sop.getNumXTiles() ||
        tileY < 0 || tileY >= sop.getNumYTiles())
      throw new IndexOutOfBoundsException();

    this.sop = sop;

    final int tw = sop.getTileWidth();
    final int th = sop.getTileHeight();
    final int sw = sop.getWidth();
    final int sh = sop.getHeight();

    x0 = tileX*tw;
    y0 = tileY*th;
    x1 = Math.min((tileX+1)*tw, sw);
    y1 = Math.min((tileY+1)*th, sh);

    size = new Dimension(x1-x0, y1-y0);

    hash = new HashCodeBuilder().append(sop)
                                .append(x0)
                                .append(y0)
                                .append(x1)
                                .append(y1)
                                .toHashCode();
  }

  public List<Op<?>> getSources() {
    return Collections.emptyList();
  }

  public BufferedImage eval() throws ImageIOException {
// FIXME: getting archive this way is a kludge, we should get it from sop
    final DataArchive archive = GameModule.getGameModule().getDataArchive();
    final String name = getName();

    try {
      final SVGRenderer renderer = new SVGRenderer(
        archive.getURL(name),
        new BufferedInputStream(archive.getInputStream(name))
      );

      final Rectangle2D aoi = new Rectangle2D.Float(x0, y0, x1-x0, y1-y0);
      return renderer.render(0.0, 1.0, aoi);
    }
    catch (FileNotFoundException e) {
      throw new ImageNotFoundException(name, e);
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
  }

  protected void fixSize() { }

  public String getName() {
    return sop.getName();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || o.getClass() != this.getClass()) return false;

    final SourceTileOpSVGImpl op = (SourceTileOpSVGImpl) o;
    return x0 == op.x0 &&
           y0 == op.y0 &&
           x1 == op.x1 &&
           y1 == op.y1 &&
           sop.equals(op.sop);
  }

  @Override
  public int hashCode() {
    return hash;
  }
}
