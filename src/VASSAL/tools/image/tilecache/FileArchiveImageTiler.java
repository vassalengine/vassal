/*
 * $Id$
 *
 * Copyright (c) 2010, 2011 by Joel Uckelman
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
import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.ExecutorService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.image.ImageLoader;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.lang.Callback;

/**
 * Tiles images contained in a ZIP archive.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class FileArchiveImageTiler {
  private static final Logger logger =
    LoggerFactory.getLogger(FileArchiveImageTiler.class);

  /**
   * Tile image contained in an archive.
   *
   * @param fa the file archive
   * @param tpath path to the output directory
   * @param tw tile width, in pixels
   * @param th tile height, in pixels
   * @param ipaths paths within the archive to images to be tiled
   */
  public void run(
    FileArchive fa,
    final String tpath,
    int tw,
    int th,
    String[] ipaths,
    ExecutorService exec,
    ImageLoader loader,
    TileSlicer slicer,
    Callback<String> imageListener,
    Callback<Void> tileListener,
    Callback<Void> doneListener
  ) throws IOException
  {
    for (String ipath : ipaths) {
      logger.info("Tiling {}", ipath);
      imageListener.receive(ipath);

      BufferedImage src = null;
      InputStream in = null;
      try {
        in = fa.getInputStream(ipath);
        src = loader.load(
          ipath, in, BufferedImage.TYPE_INT_RGB,
          BufferedImage.TYPE_INT_ARGB_PRE, false
        );
        in.close();
      }
      catch (IOException e) {
        logger.error("", e);
        continue;
      }
      finally {
        IOUtils.closeQuietly(in);
      }

      slicer.slice(src, ipath, tpath, tw, th, exec, tileListener);
    }

    exec.shutdown();
    doneListener.receive(null);
  }
}
