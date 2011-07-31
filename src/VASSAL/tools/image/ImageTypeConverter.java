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

import java.awt.image.BufferedImage;

import VASSAL.tools.lang.Reference;

/**
 * An interface for converting {@link BufferedImage}s from one type to
 * another.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public interface ImageTypeConverter {
  /**
   * Converts an image to the given type.
   *
   * @param ref a holder for the image
   * @param type the type of image to return
   * @return a converted image
   *
   * @throws ImageIOException if something goes wrong
   */
  public BufferedImage convert(Reference<BufferedImage> ref, int type)
                                                       throws ImageIOException;
}
