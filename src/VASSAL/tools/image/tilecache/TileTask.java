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

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.Callable;

/**
 * Slices one tile from an image and writes it to disk.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
class TileTask implements Callable<Void> {
  protected final BufferedImage src;
  protected final File dst;
  protected final int tx;
  protected final int ty;
  protected final int tw;
  protected final int th;
  protected final int dw;
  protected final int dh;

  /**
   * @param src the source image
   * @param dst the destination file
   * @param tx the tile column
   * @param ty the tile row
   * @param tw the standard tile width
   * @param th the standard tile height
   * @param dw the width of the whole destination image
   * @param dh the height of the whole destination image
   */
  public TileTask(BufferedImage src, File dst,
                  int tx, int ty, int tw, int th, int dw, int dh) {
    this.src = src;
    this.dst = dst;
    this.tx = tx;
    this.ty = ty;
    this.tw = tw;
    this.th = th;
    this.dw = dw;
    this.dh = dh;
  }

  /** {@inheritDoc} */
  public Void call() throws IOException {
    final BufferedImage tile = sliceTile();
    TileUtils.write(tile, dst);
    return null;
  }

  protected BufferedImage sliceTile() {
    // get actual tile width, height (edge tiles can be less than full size)
    final int atw = Math.min(tw, dw - tx*tw);
    final int ath = Math.min(th, dh - ty*th);

    final int type = src.getType();

    // slice the tile from the source image
    final BufferedImage tile = new BufferedImage(atw, ath, type);

    final Graphics2D g = tile.createGraphics();
    g.drawImage(src, 0, 0, atw, ath,
                     tx*tw, ty*th, tx*tw+atw, ty*th+ath, null);
    g.dispose();

    return tile;
  }
}
