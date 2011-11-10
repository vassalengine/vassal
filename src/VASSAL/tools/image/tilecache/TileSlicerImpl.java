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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import VASSAL.tools.image.GeneralFilter;
import VASSAL.tools.lang.Callback;

/**
 * Slices an image into tiles.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class TileSlicerImpl implements TileSlicer {
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
  ) throws IOException
  {
    final int sw = src.getWidth();
    final int sh = src.getHeight();

    final List<Future<Void>> futures = new ArrayList<Future<Void>>();

    // slice unscaled 1:1 tiles
    final TaskMaker unscaled = new TaskMaker() {
      public TileTask make(BufferedImage src, File f,
                           int tx, int ty, int tw, int th, int sw, int sh) {
        return new TileTask(src, f, tx, ty, tw, th, sw, sh);
      }
    };

    queueTileTasks(
      src, iname, tpath, 1, tw, th, sw, sh, unscaled, exec, futures
    );

    // slice scaled tiles, starting at 1:2
    final TaskMaker scaled = new TaskMaker() {
      private final GeneralFilter.Filter filter =
        new GeneralFilter.Lanczos3Filter();

      public TileTask make(BufferedImage src, File f,
                           int tx, int ty, int tw, int th, int dw, int dh) {
        return new ScaledTileTask(src, f, filter, tx, ty, tw, th, dw, dh);
      }
    };

    for (int div = 2; sw/div > 0 && sh/div > 0; div <<= 1) {
      final int dw = sw/div;
      final int dh = sh/div;

      queueTileTasks(
        src, iname, tpath, div, tw, th, dw, dh, scaled, exec, futures
      );
    }

    // wait for all tiles to complete
    try {
      for (Future<Void> f : futures) {
        f.get();
        progress.receive(null);
      }
    }
    catch (CancellationException e) {
      // should never happen
      throw new IllegalStateException(e);
    }
    catch (ExecutionException e) {
      throw (IOException) new IOException().initCause(e);
    }
    catch (InterruptedException e) {
      // should never happen
      throw new IllegalStateException(e);
    }
    finally {
      // cancel everything if anything fails
      for (Future<Void> f : futures) {
        if (!f.isDone()) f.cancel(true);
      }
    }
  }

  protected static interface TaskMaker {
    public TileTask make(BufferedImage src, File f,
                         int tx, int ty, int tw, int th, int dw, int dh);
  }

  protected static void queueTileTasks(
    BufferedImage src,
    String iname,
    String tpath,
    int div,
    int tw,
    int th,
    int dw,
    int dh,
    TaskMaker tm,
    ExecutorService exec,
    List<Future<Void>> futures
  )
  {
    final int tcols = (int) Math.ceil((double) dw / tw);
    final int trows = (int) Math.ceil((double) dh / th);

    for (int tx = 0; tx < tcols; ++tx) {
      for (int ty = 0; ty < trows; ++ty) {
        final String tn = TileUtils.tileName(iname, tx, ty, div);
        final File f = new File(tpath, tn);

        final TileTask tt = tm.make(src, f, tx, ty, tw, th, dw, dh);
        futures.add(exec.submit(tt));
      }
    }
  }
}
