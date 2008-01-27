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

package VASSAL.tools.imageop;

import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;

import VASSAL.tools.memmap.MappedBufferedImage;

/**
 * An {@link ImageOp} which loads an image from the {@link DataArchive}
 * into a {@link MappedBufferedImage}. This class is useful for loading
 * very large images, like ones used by
 * {@link VASSAL.build.module.map.boardPicker.Board}, in such a way
 * which consumes little RAM in the long-term.
 *
 * @see MappedBufferedImage
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class MemoryMappedSourceOp extends SourceOp { 
  // hold a ref to the image so it's never uncached
  private BufferedImage dst;

  /**
   * Constructs an <code>ImageOp</code> which will load the given image
   * file into to a memory-mapped file-backed <code>Image</code>.
   *
   * @param name the name of the image to load
   * @throws IllegalArgumentException
   *    if <code>name</code> is <code>null</code>.
   */
  public MemoryMappedSourceOp(String name) {
    super(name);
  }

  /** {@inheritDoc} */
  @Override
  protected Image apply() throws IOException {
    final Image src = super.apply();

    // System.out.print(getName() + ": ");
    dst = new MappedBufferedImage(src.getWidth(null), src.getHeight(null));
    final Graphics2D g = dst.createGraphics();
    g.drawImage(src, 0, 0, null);
    g.dispose();
    
    return dst;
  }
}
