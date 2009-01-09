/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman 
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

import java.awt.image.BufferedImage;
import java.io.InputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

import VASSAL.tools.DataArchive;
import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageNotFoundException;
import VASSAL.tools.image.memmap.MappedImageUtils;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class SourceOpMappedBitmapImpl extends SourceOpBitmapImpl
                                      implements SourceOp {
  public SourceOpMappedBitmapImpl(String name) {
    super(name);
  }
  
  public SourceOpMappedBitmapImpl(String name, DataArchive archive) {
    super(name, archive);
  }

  /**
   *  {@inheritDoc}
   *
   * @throws IOException if the image cannot be loaded from the image file.
   */
  @Override
  public BufferedImage eval() throws ImageIOException {
    InputStream in = null;
    try {
      in = archive.getImageInputStream(name);
    }
    catch (FileNotFoundException e) {
      throw new ImageNotFoundException(name, e);
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }

    return MappedImageUtils.getImage(name, in);
  }
}
