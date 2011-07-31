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

package VASSAL.tools.image;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

import VASSAL.tools.lang.Reference;

/**
 * Convert a {@link BufferedImage} to a different type, in memory.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class MemoryImageTypeConverter implements ImageTypeConverter {

  /** {@inheritDoc} */
  public BufferedImage convert(Reference<BufferedImage> ref, int type)
                                                      throws ImageIOException {
    if (ref == null) throw new IllegalArgumentException();

    // NB: We don't bother clearing the ref because this method requires
    // that the source and destination images exist simultaneously.

    final BufferedImage src = ref.obj;

    // we can't create images of TYPE_CUSTOM
    if (type == BufferedImage.TYPE_CUSTOM) throw new IllegalArgumentException();

    final int w = src.getWidth();
    final int h = src.getHeight();

    final BufferedImage dst = new BufferedImage(w, h, type);

    final Graphics2D g = dst.createGraphics();
    g.drawImage(src, 0, 0, null);
    g.dispose();

    return dst;
  }
}
