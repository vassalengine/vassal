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
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.List;

import VASSAL.tools.HashCode;
import VASSAL.tools.ImageUtils;

/**
 * An {@link ImageOp} for producing tiles directly from a source,
 * without cobbling tiles from the source.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class SourceTileOpBitmapImpl extends AbstractTileOpImpl {
  private final ImageOp sop;
  private final int x0, y0, x1, y1;
  private final int hash;
 
  public SourceTileOpBitmapImpl(ImageOp sop, int tileX, int tileY) {
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

    final int PRIME = 31;
    int result = 1;
    result = PRIME * result + HashCode.hash(sop);
    result = PRIME * result + HashCode.hash(x0);
    result = PRIME * result + HashCode.hash(y0);
    result = PRIME * result + HashCode.hash(x1);
    result = PRIME * result + HashCode.hash(y1);
    hash = result;
  }

  public List<VASSAL.tools.opcache.Op<?>> getSources() {
    return Collections.<VASSAL.tools.opcache.Op<?>>singletonList(sop);
  }

  public BufferedImage eval() throws Exception {
    final BufferedImage src = sop.getImage(null);
    final BufferedImage dst = ImageUtils.createCompatibleImage(
      size.width, size.height, src.getTransparency() != BufferedImage.OPAQUE
    );

    final Graphics2D g = dst.createGraphics();
    g.drawImage(src, 0, 0, size.width, size.height, x0, y0, x1, y1, null);
    g.dispose();

    return dst;
  }

  protected void fixSize() { }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || !(o instanceof SourceTileOpBitmapImpl)) return false;

    final SourceTileOpBitmapImpl op = (SourceTileOpBitmapImpl) o;
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
