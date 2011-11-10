/*
 * $Id$
 *
 * Copyright (c) 2007-2010 by Joel Uckelman
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

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.Graphics2D;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;

public class GeneralFilterTest {
  /** A program for running filter benchmarks. */
  public static void main(String[] args) throws IOException {
    BufferedImage src = ImageIO.read(new File(args[0]));
    final float scale = Float.parseFloat(args[1]);

    final int dw = (int) (src.getWidth() * scale);
    final int dh = (int) (src.getHeight() * scale);

    int type;
    switch (Integer.parseInt(args[2])) {
    case 0: type = BufferedImage.TYPE_INT_ARGB; break;
    case 1: type = BufferedImage.TYPE_INT_ARGB_PRE; break;
    case 2: type = BufferedImage.TYPE_INT_RGB; break;
    default: throw new IllegalArgumentException();
    }

    final BufferedImage tmp =
      new BufferedImage(src.getWidth(), src.getHeight(), type);

    final Graphics2D g = tmp.createGraphics();
    g.drawImage(src, 0, 0, null);
    g.dispose();

    src = tmp;

    for (long t : run(src, dw, dh, 100)) {
      System.out.println(t);
    }

    System.out.println("Ready...");
    System.in.read();
    System.out.println("Starting...");

    long acc = 0;
    for (long t : run(src, dw, dh, 100)) {
      acc += t;
    }

    System.out.println((double) acc/100);

    System.out.println("Done.");
    System.in.read();
  }

  protected static long[] run(BufferedImage src, int dw, int dh, int times) {
    final GeneralFilter.Filter filter = new GeneralFilter.Lanczos3Filter();

    BufferedImage dst;

    final long[] time = new long[times];

    for (int i = 0; i < times; ++i) {
      final long start = System.currentTimeMillis();
      dst = GeneralFilter.zoom(new Rectangle(0, 0, dw, dh), src, filter);
      time[i] = System.currentTimeMillis() - start;
    }

    return time;
  }
}
