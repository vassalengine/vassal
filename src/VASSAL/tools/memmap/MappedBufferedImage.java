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

package VASSAL.tools.memmap;

import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.io.IOException;

/**
 * A {@link BufferedImage} backed by a memory-mapped file.
 * <code>MappedBufferedImage</code>s are useful for storing images
 * on disk which are too large to keep in RAM long-term but for which
 * decoding them at each access would be too slow. The image data
 * is held by a {@link MappedWritableRaster}, which in turn stores the
 * data in a {@link MappedDataBufferInt}. A <code>MappedBufferedImage</code>
 * acts as though it has type {@link #TYPE_INT_ARGB}.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @see MappedDataBufferInt
 * @see MappedWritableRaster
 * @see MappedSinglePixelPackedSampleModel
 */
public class MappedBufferedImage extends BufferedImage {
  /** 
   * Constructs a <code>MappedBufferedImage</code> with the given
   * width and height.
   *
   * @param w width of the created image
   * @param h height of the created image
   * @throws IOException if creating the memory-mapped file fails.
   */
  public MappedBufferedImage(int w, int h) throws IOException {
    super(new DirectColorModel(32, 0x00ff0000, 0x0000ff00,
                                   0x000000ff, 0xff000000),
          MappedWritableRaster.createPackedRaster(
                               new MappedDataBufferInt(4*w*h),
                               w, h, w,
                               new int[]{0x00ff0000, 0x0000ff00,
                                         0x000000ff, 0xff000000},
                               null),
          false,
          null
    );
  }
}
