/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.imageop;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.awt.Dimension;
import java.awt.image.BufferedImage;

import VASSAL.tools.image.ImageUtils;

import org.junit.jupiter.api.Test;

/**
 * A board scaled far enough down has no pixels left to cobble a tile from,
 * and the renderer used to walk off the end of an empty array doing it.
 *
 * @see <a href="https://github.com/vassalengine/vassal/issues/15298">#15298</a>
 */
public class TiledScaleOpZeroSourceTest {

  private static ImageOp source(int w, int h) {
    return new ImageSourceOpBitmapImpl(new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB));
  }

  /**
   * The premise: a scaled op's size is truncated rather than rounded up, so a
   * dimension of under one pixel becomes zero rather than one.
   */
  @Test
  public void aScaledSizeCanTruncateToZero() {
    final Dimension d =
      new ScaleOpTiledBitmapImpl(source(100, 100), 1.0 / 128).getSize();
    assertEquals(new Dimension(0, 0), d,
      "100 px at 1/128 is under a pixel, and truncates to none"); //NON-NLS
  }

  /**
   * Scale chosen so the pyramid level below it is 1/128: 1/0.005 is 200, which
   * is in [128, 256). The source is then 0 x 0, there are no tiles to cobble
   * from, and this threw ArrayIndexOutOfBoundsException from the repaint.
   */
  @Test
  public void fixedScaleTileOfAZeroSizedSourceIsEmptyNotAnError() throws Exception {
    final FixedScaleOpTiledBitmapImpl op =
      new FixedScaleOpTiledBitmapImpl(source(100, 100), 0.005, 10, 10);
    op.getNumXTiles();   // force the lazy tile grid, as the render path does
    assertSame(ImageUtils.NULL_IMAGE, op.getTileOp(0, 0).eval());
  }

  /**
   * Only the fixed-scale op can get into this state. Its destination size is
   * whatever the constructor was handed, so it can still have a tile to draw
   * when its source has scaled to nothing. ScaleOpTiledBitmapImpl derives its
   * size from the source, so the source cannot reach zero before it does, and
   * by then the op has no tiles for anyone to ask about. Its copy of the cobble
   * loop carries the same guard because it is the same code, but there is no
   * test for it here: it cannot be reached by this route.
   */
  @Test
  public void aScaleOpGoesToZeroBeforeItsSourceDoes() {
    assertEquals(new Dimension(0, 0),
      new ScaleOpTiledBitmapImpl(source(100, 100), 0.005).getSize(),
      "the op is empty before its source is, so it has no tiles to render"); //NON-NLS
  }
}
