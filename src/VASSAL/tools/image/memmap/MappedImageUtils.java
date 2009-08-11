/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

package VASSAL.tools.image.memmap;

import java.io.IOException;
import java.io.InputStream;

import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageUtils;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */ 
public class MappedImageUtils {

  private MappedImageUtils() {}

  public static MappedBufferedImage getImage(String name, InputStream in)
                                                      throws ImageIOException {
    return MappedImageLoader.getImage(name, in);
  }

  public static MappedBufferedImage createCompatibleImage(int w, int h) {
    return new MappedBufferedImage(w, h, ImageUtils.getCompatibleImageType());
  }

  public static MappedBufferedImage createCompatibleImage(int w, int h,
                                                    boolean transparent) {
    return new MappedBufferedImage(w, h,
      ImageUtils.getCompatibleImageType(transparent));
  }

  public static MappedBufferedImage createCompatibleTranslucentImage(
                                                                int w, int h) {
    return new MappedBufferedImage(w, h,
      ImageUtils.getCompatibleTranslucentImageType());
  }

  public static MappedBufferedImage toCompatibleImage(MappedBufferedImage src)
                                                           throws IOException {
    if (ImageUtils.isCompatibleImage(src)) return src;

    return toType(src,
      ImageUtils.getCompatibleImageType(ImageUtils.isTransparent(src)));
  }

  public static MappedBufferedImage toType(MappedBufferedImage src, int type)
                                                           throws IOException {
    return rowByRowCopy(
      src, 
      new MappedBufferedImage(src.getWidth(), src.getHeight(), type)
    );
  }

  private static MappedBufferedImage rowByRowCopy(MappedBufferedImage src,
                                                  MappedBufferedImage dst) {
    final int h = src.getHeight();
    final int[] row = new int[src.getWidth()];
    for (int y = 0; y < h; ++y) {
      src.getRGB(0, y, row.length, 1, row, 0, row.length);
      dst.setRGB(0, y, row.length, 1, row, 0, row.length);
    }
    return dst;
  }
}
