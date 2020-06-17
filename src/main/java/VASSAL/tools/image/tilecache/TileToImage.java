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

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.imageio.ImageIO;

/**
 * Converts tile files to image files.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class TileToImage {
  /**
   * Converts a tile file to an image file.
   *
   * @param args the first argument is the path of the source tile file,
   * the second is the path of the PNG to write
   *
   * @throws IOException if something goes wrong
   */
  public static void main(String[] args) throws IOException {
    // Oh we have no heads, we have no HEADS!
    System.setProperty("java.awt.headless", "true");

    final File tfile = new File(args[0]);
    final BufferedImage img = TileUtils.read(tfile);
    ImageIO.write(img, "PNG", new FileOutputStream(args[1]));
  }
}
