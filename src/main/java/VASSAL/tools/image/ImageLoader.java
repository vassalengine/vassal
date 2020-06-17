/*
 * $Id$
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
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

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.InputStream;

/**
 * An interface for loading images.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public interface ImageLoader {
  /**
   * Loads an image.
   *
   * @param name the image name
   * @param in the input stream
   * @param typeIfOpaque the requested image type for opaque images
   * @param typeIfTransparent the requested image type for transparent images
   * @param managed <code>true</code> if a managed image should be returned
   * @return the image
   *
   * @throws BrokenImageException if the image is faulty
   * @throws UnrecognizedImageTypeException if the image type is not recognized
   * @throws ImageIOException if reading the image goes wrong
   */
  public BufferedImage load(
    String name,
    InputStream in,
    int typeIfOpaque,
    int typeIfTransparent,
    boolean managed
  ) throws ImageIOException;

  /**
   * Gets the size of an image.
   *
   * @param name the image name
   * @param in the input stream
   * @return the size of the image
   *
   * @throws BrokenImageException if the image is faulty
   * @throws UnrecognizedImageTypeException if the image type is not recognized
   * @throws ImageIOException if reading the image goes wrong
   */
  public Dimension size(String name, InputStream in) throws ImageIOException;
}
