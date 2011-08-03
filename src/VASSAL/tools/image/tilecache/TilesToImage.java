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

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.imageio.ImageIO;

/**
 * Reconstitute an image from tile files.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class TilesToImage {
  /**
   * Converts tile files to an image file.
   *
   * @param args the first argument is the path of the basename of the tile
   * files, the second argument is the scale factor, the third is the path
   * of the PNG to write
   *
   * @throws IOException if something goes wrong
   */
  public static void main(String[] args) throws IOException {
    // Oh we have no heads, we have no HEADS!
    System.setProperty("java.awt.headless", "true");

    final String base = args[0];
    final String scale = args[1];
    final String dpath = args[2];

    Dimension d;

    // find the tile in the upper left corner to get tile size
    d = TileUtils.size(base + "(0,0)@1:" + scale);

    final int tw = d.width;
    final int th = d.height;

    // find the tile in the lower right corner to get the number of rows
    // and columns, as well as the width of the last column and the height
    // of the last row
    final File bdir = new File(base).getParentFile();
    final FileFilter filter = new FileFilter() {
      public boolean accept(File pathname) {
        return pathname.getPath().startsWith(base);
      }
    };

    final Pattern p  = Pattern.compile(base + "\\((\\d+),(\\d+)\\)@1:");
    int max_row = 0;
    int max_col = 0;
    for (File f : bdir.listFiles(filter)) {
      final Matcher m = p.matcher(f.getPath());
      if (m.lookingAt()) {
        final int c = Integer.parseInt(m.group(1));
        final int r = Integer.parseInt(m.group(2));

        if (c > max_col) max_col = c;
        if (r > max_row) max_row = r;
      }
    }

    final int tcols = max_col + 1;
    final int trows = max_row + 1;

    d = TileUtils.size(base + "(" + max_col + "," + max_row + ")@1:" + scale);

    final int tw_last = d.width;
    final int th_last = d.height;

    // create the new image
    final int w = tw*(tcols-1) + tw_last;
    final int h = th*(trows-1) + th_last;

    final BufferedImage img =
      new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);

    final Graphics2D g = img.createGraphics();

    // TODO: We could do this faster by making it multithreaded, since
    // writes to the destination image never overlap.
    for (int tx = 0; tx < tcols; ++tx) {
      for (int ty = 0; ty < trows; ++ty) {
        final File tfile =
          new File(base + "(" + tx + "," + ty + ")@1:" + scale);
        final BufferedImage tile = TileUtils.read(tfile);

        g.drawImage(tile, tx*tw, ty*th, null);
      }
    }

    // write the cobbled image
    ImageIO.write(img, "PNG", new FileOutputStream(dpath));
  }
}
