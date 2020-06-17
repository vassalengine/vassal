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
import java.io.IOException;
import java.util.concurrent.ExecutorService;

import VASSAL.tools.lang.Callback;

/**
 * Slices an image into tiles.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public interface TileSlicer {
  /**
   * Slices an image into tiles.
   *
   * @param src the source image
   * @param iname the basename for the tiles
   * @param tpath the path for the tiles
   * @param tw the tile width
   * @param th the tile height
   * @param exec the executor in which to run tasks
   * @param progress a callback for indicating progress
   */
  public void slice(
    BufferedImage src,
    String iname,
    String tpath,
    int tw,
    int th,
    ExecutorService exec,
    Callback<Void> progress
  ) throws IOException;
}
