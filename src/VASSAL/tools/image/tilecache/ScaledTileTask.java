/*
 * $Id$
 *
 * Copyright (c) 2010 by Joel Uckelman
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

package VASSAL.tools.image.tilecache;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.io.File;

import VASSAL.tools.image.GeneralFilter;

/**
 * Slices one scaled tile from an image and writes it to disk.
 */
class ScaledTileTask extends TileTask {
  protected final GeneralFilter.Filter filter;

  /**
   * Creates a scaled tile task.
   *
   * @param src the source image
   * @param filter the resampling filter
   * @param dst the destination file
   * @param tx the tile column
   * @param ty the tile row
   * @param tw the standard tile width
   * @param th the standard tile height
   * @param dw the width of the whole scaled image
   * @param dh the height of the whole scaled image
   */
  public ScaledTileTask(BufferedImage src, File dst,
                        GeneralFilter.Filter filter,
                        int tx, int ty, int tw, int th, int dw, int dh) {
    super(src, dst, tx, ty, tw, th, dw, dh);
    this.filter = filter;
  }

  @Override
  protected BufferedImage sliceTile() {
    // get actual tile width, height (edge tiles can be less than full size)
    final int atw = Math.min(tw, dw - tx*tw);
    final int ath = Math.min(th, dh - ty*th);

    final int type = src.getType();

    // scale the tile from the source image
    final BufferedImage tile = new BufferedImage(atw, ath, type);

    final WritableRaster tileR =
      tile.getRaster().createWritableTranslatedChild(tx*tw, ty*th);
    final Rectangle dstFR = new Rectangle(0, 0, dw, dh);

    GeneralFilter.zoom(tileR, dstFR, src, filter);

    return tile;
  }
}
