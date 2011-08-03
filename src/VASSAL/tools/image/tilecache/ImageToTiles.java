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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import VASSAL.tools.concurrent.DaemonThreadFactory;
import VASSAL.tools.image.FallbackImageTypeConverter;
import VASSAL.tools.image.ImageIOImageLoader;
import VASSAL.tools.image.ImageLoader;
import VASSAL.tools.image.ImageTypeConverter;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.TemporaryFileFactory;
import VASSAL.tools.lang.Callback;

/**
 * Converts an image file to tile files.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class ImageToTiles {
  /**
   * Converts an image file to tile files.
   *
   * @param args the first argument is the path of the source image file,
   * the second argument is the destination path for the tile files, the
   * third and fourth arguments are the tile width and height
   *
   * @throws IOException if someting goes wrong
   */
  public static void main(String[] args) throws IOException {
    // Oh we have no heads, we have no HEADS!
    System.setProperty("java.awt.headless", "true");

    final String ipath = args[0];
    final String tpath = args[1];
    final int tw = Integer.parseInt(args[2]);
    final int th = Integer.parseInt(args[3]);

    // TODO: Determine what the optimal number of threads is.
    final Runtime runtime = Runtime.getRuntime();
    final ExecutorService exec = new ThreadPoolExecutor(
      runtime.availableProcessors(),
      runtime.availableProcessors()+1,
      60, TimeUnit.SECONDS,
      new LinkedBlockingQueue<Runnable>(),
      new DaemonThreadFactory(ImageToTiles.class.getSimpleName())
    );

    final TemporaryFileFactory tfac = new TemporaryFileFactory() {
      public File create() throws IOException {
        return File.createTempFile("img", null, new File(tpath));
      }
    };

    final ImageTypeConverter itc = new FallbackImageTypeConverter(tfac);
    final ImageLoader loader = new ImageIOImageLoader(itc);

    BufferedImage src = null;
    InputStream in = null;
    try {
      in = new FileInputStream(ipath);
      src = loader.load(
        ipath, in, BufferedImage.TYPE_INT_RGB,
        BufferedImage.TYPE_INT_ARGB_PRE, false
      );
      in.close();
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    final String iname = new File(ipath).getName();
    final Callback<Void> dotter = new Callback<Void>() {
      public void receive(Void obj) {
        System.out.print('.');
      }
    };

    final TileSlicer slicer = new TileSlicerImpl();

    slicer.slice(src, iname, tpath, tw, th, exec, dotter);
    exec.shutdown();
    System.out.println("");
  }
}
