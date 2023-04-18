/*
 * Copyright (c) 2010-2021 by Joel Uckelman
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

package VASSAL.launch;

import VASSAL.Info;
import VASSAL.i18n.Resources;
import VASSAL.tools.DataArchive;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.image.tilecache.ImageTileDiskCache;
import VASSAL.tools.image.tilecache.TileUtils;
import VASSAL.tools.io.ArgEncoding;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.FileStore;
import VASSAL.tools.io.InputOutputStreamPump;
import VASSAL.tools.io.InputStreamPump;
import VASSAL.tools.io.ProcessLauncher;
import VASSAL.tools.io.ProcessWrapper;
import VASSAL.tools.lang.MemoryUtils;
import VASSAL.tools.lang.Pair;
import VASSAL.tools.swing.EDT;
import VASSAL.tools.swing.ProgressDialog;
import VASSAL.tools.swing.Progressor;

import org.apache.commons.io.FileUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.awt.Dimension;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.DONE;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.IMAGE_BEGIN;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.IMAGE_END;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.READY;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILE_END;

/**
 * A launcher for the process which tiles large images.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class TilingHandler {
  private static final Logger logger =
    LoggerFactory.getLogger(TilingHandler.class);

  protected final String aname;
  protected final File cdir;
  protected final Dimension tdim;
  protected final int maxheap_limit;

  // Needed for VASL. Remove sometime after VASL 6.6.2
  @Deprecated(since = "2021-12-01", forRemoval = true)
  @SuppressWarnings("PMD.FinalFieldCouldBeStatic")
  protected final int pid = 42;

  /**
   * Creates a {@code TilingHandler}.
   *
   * @param aname the path to the ZIP archive
   * @param cdir the tile cache directory
   * @param tdim the tile size
   */
  public TilingHandler(String aname, File cdir, Dimension tdim, int mhlim) {
    this.aname = aname;
    this.cdir = cdir;
    this.tdim = tdim;
    this.maxheap_limit = mhlim;
  }

  /**
   * @deprecated Retained for VASL compatibility. Remove sometime after
   * VASL 6.6.2.
   */
  @Deprecated(since = "2021-12-01", forRemoval = true)
  @SuppressWarnings("PMD.UnusedFormalParameter")
  public TilingHandler(String aname, File cdir,
                       Dimension tdim, int mhlim, int pid) {
    this(aname, cdir, tdim, mhlim);
  }

  protected boolean isFresh(
    FileArchive archive,
    FileStore tcache,
    String ipath,
    Dimension idim) throws IOException {

    final long imtime = archive.getMTime(ipath);

    if (imtime < 0) {
      // time in archive might be goofy
      return false;
    }

    final int iw = idim.width;
    final int ih = idim.height;
    final int tw = tdim.width;
    final int th = tdim.height;

    // iterate over all the tiles
    for (int div = 1; iw / div > 0 && ih / div > 0; div <<= 1) {
      final int cols = (int) Math.ceil((double) (iw / div) / tw);
      final int rows = (int) Math.ceil((double) (ih / div) / th);

      for (int c = 0; c < cols; ++c) {
        for (int r = 0; r < rows; ++r) {
          final String tpath = TileUtils.tileName(ipath, c, r, div);

          // check whether the tile is newer than the image
          if (imtime >= tcache.getMTime(tpath)) {
            return false;
          }
        }
      }
    }

    // all tiles exist and are newer than the image
    return true;
  }

  @Deprecated(since = "2023-04-17", forRemoval = true)
  protected boolean isFresh(FileArchive archive,
                            FileStore tcache, String ipath)
                                                           throws IOException {
    // look at the first 1:1 tile
    final String tpath = TileUtils.tileName(ipath, 0, 0, 1);

    // check whether the image is older than the tile
    final long imtime = archive.getMTime(ipath);

    return imtime > 0 && // time in archive might be goofy
           imtime <= tcache.getMTime(tpath);
  }

  protected Dimension getImageSize(DataArchive archive, String ipath)
                                                           throws IOException {
    try (InputStream in = archive.getInputStream(ipath)) {
      return ImageUtils.getImageSize(ipath, in);
    }
  }

  protected Pair<Integer, Integer> findImages(
    DataArchive archive,
    FileStore tcache,
    List<String> multi,
    List<Pair<String, IOException>> failed) throws IOException {

    // build a list of all multi-tile images and count tiles
    final Set<String> images = archive.getImageNameSet(true, true);

    int maxpix = 0; // number of pixels in the largest image
    int tcount = 0; // tile count

    final FileArchive fa = archive.getArchive();

    for (final String ipath : images) {
      final Dimension idim;
      try {
        idim = getImageSize(archive, ipath);
      }
      catch (IOException e) {
        // skip images we can't read
        failed.add(Pair.of(ipath, e));
        continue;
      }

      // skip images with fresh tiles
      if (isFresh(fa, tcache, ipath, idim)) {
        continue;
      }

      // count the tiles at all sizes if we have more than one tile at 1:1
      final int t = TileUtils.tileCountAtScale(idim, tdim, 1) > 1 ?
                    TileUtils.tileCount(idim, tdim) : 0;

      if (t == 0) continue;

      tcount += t;
      multi.add(ipath);

      // check whether this image has the most pixels
      if (idim.width * idim.height > maxpix) {
        maxpix = idim.width * idim.height;
      }
    }

    return new Pair<>(tcount, maxpix);
  }

  protected interface StateMachineHandler {
    void handleStart();

    default void handleRestart(Future<Integer> fut) {}

    void handleStartingImageState(String in);

    default void handleFinishedImageState(String in) {}

    void handleTileWrittenState();

    void handleTilingFinishedState();

    void handleSuccess();

    void handleFailure();
  }

  private static class MyStateMachineHandler implements StateMachineHandler {
    private Future<Integer> fut;
    private final int tcount;
    private final ProgressDialog pd;
    private final Progressor progressor;

    public MyStateMachineHandler(int tcount, Future<Integer> fut) {
      this.tcount = tcount;
      this.fut = fut;

      // get the progress dialog
      pd = ProgressDialog.createOnEDT(
        ModuleManagerWindow.getInstance(),
        Resources.getString("TilingHandler.processing_image_tiles"),
        " "
      );

      progressor = new Progressor(0, tcount) {
        @Override
        protected void run(Pair<Integer, Integer> prog) {
          pd.setProgress((100 * prog.second) / max);
        }
      };
    }

    @Override
    public void handleStart() {
      // setup the cancel button in the progress dialog
      EDT.execute(() -> pd.addActionListener(e -> {
        pd.setVisible(false);
        fut.cancel(true);
      }));
    }

    @Override
    public void handleRestart(Future<Integer> f) {
      fut = f;
    }

    @Override
    public void handleStartingImageState(String ipath) {
      EDT.execute(() -> {
        pd.setLabel(Resources.getString("TilingHandler.tiling", ipath));
        if (!pd.isVisible()) {
          pd.setVisible(true);
        }
      });
    }

    @Override
    public void handleFinishedImageState(String ipath) {
    }

    @Override
    public void handleTileWrittenState() {
      progressor.increment();

      if (progressor.get() >= tcount) {
        pd.setVisible(false);
      }
    }

    @Override
    public void handleTilingFinishedState() {
    }

    @Override
    public void handleSuccess() {
    }

    @Override
    public void handleFailure() {
      pd.setVisible(false);
    }
  }

  protected String[] makeSlicerCommandLine(int heap) {
    final List<String> args = new ArrayList<>(List.of(
      Info.getJavaBinPath().getAbsolutePath(),
      "-classpath", //NON-NLS
      System.getProperty("java.class.path"),
      "-Xmx" + heap + "M", //NON-NLS
      "-Duser.home=" + System.getProperty("user.home"), //NON-NLS
      "VASSAL.tools.image.tilecache.ZipFileImageTiler"
    ));

    final String cdirpath = cdir.getAbsolutePath();

    if (ArgEncoding.requires(aname) || ArgEncoding.requires(cdirpath)) {
      args.add("--encoded-args");
      args.add(ArgEncoding.encode(aname));
      args.add(ArgEncoding.encode(cdirpath));
    }
    else {
      args.add(aname);
      args.add(cdirpath);
    }

    args.add(String.valueOf(tdim.width));
    args.add(String.valueOf(tdim.height));

    return args.toArray(new String[0]);
  }

  protected StateMachineHandler createStateMachineHandler(int tcount, Future<Integer> fut) {
    return new MyStateMachineHandler(tcount, fut);
  }

  protected static final int SLICER_SUCCESS = -1;
  protected static final int SLICER_GAVE_UP = -2;

  protected Pair<Integer, Integer> runSlicer(
    List<String> multi,
    int maxheap,
    StateMachineHandler h
  ) throws CancellationException, IOException {

    // don't exceed the maxheap limit
    maxheap = Math.min(maxheap, maxheap_limit);

    final String[] args = makeSlicerCommandLine(maxheap);

    // set up the process
    final InputStreamPump errP = new InputOutputStreamPump(null, System.err);

    final ProcessWrapper proc = new ProcessLauncher().launch(
      null, null, errP, args
    );

    final List<String> tiled = new ArrayList<>();

    // read state changes from child's stdout
    try (BufferedReader in = new BufferedReader(new InputStreamReader(proc.stdout, StandardCharsets.UTF_8))) {
      // This code exists because the JVM prints errors to stdout.
      // Nothing should do this in 2022. What a piece of shit.
      String line;
      while (true) {
        line = in.readLine();
        if (line == null) {
          throw new IOException("EOF before tiler started");
        }

        if (line.isEmpty()) {
          continue;
        }

        if (READY.equals(line)) {
          break;
        }
        else {
          logger.info(line);
        }
      }

      // write the image paths to child's stdin, one per line; do this in the
      // background, so that nothing is blocked if the buffer fills up
      new Thread(() -> {
        try (PrintWriter stdin = new PrintWriter(proc.stdin, true, StandardCharsets.UTF_8)) {
          multi.forEach(stdin::println);
        }
      }).start();

      h.handleRestart(proc.future);

      String imgname;
      boolean done = false;
      char type;
      while (!done && !proc.future.isCancelled()) {
        line = in.readLine();
        if (line == null) {
          throw new IOException("EOF before tiler finished");
        }

        if (line.isEmpty()) {
          continue;
        }

        type = line.charAt(0);

        switch (type) {
        case IMAGE_BEGIN:
          h.handleStartingImageState(line.substring(1));
          break;

        case IMAGE_END:
          imgname = line.substring(1);
          tiled.add(imgname);
          h.handleFinishedImageState(imgname);
          break;

        case TILE_END:
          h.handleTileWrittenState();
          break;

        case DONE:
          done = true;
          h.handleTilingFinishedState();
          break;

        default:
          logger.info(line);
        }
      }
    }
    catch (IOException e) {
      logger.error("Error during tiling", e); //NON-NLS
    }

    // wait for the tiling process to end
    try {
      final int retval = proc.future.get();
      if (retval == 0) {
        // done, don't retry
        maxheap = SLICER_SUCCESS;
      }
      else {
        proc.future.cancel(true);

        logger.info("Tiling failed with return value == " + retval + "   MaxHeap=" + maxheap + "  MaxHeapLimit=" + maxheap_limit + "  PhysMemory:" + (MemoryUtils.getPhysicalMemory() >> 20)); //NON-NLS

        if (maxheap < maxheap_limit) {
          // The tiler possibly ran out of memory; we can't reliably detect
          // this, so assume it did. Try again with 50% more max heap.
          logger.info("Tiling with " + maxheap + " possibly ran out of memory. Retrying tiling with 50% more (" + (int)(maxheap * 1.5) + ")."); //NON-NLS
          maxheap *= 1.5;
          multi.removeAll(tiled);
        }
        else {
          // give up, don't retry
          maxheap = SLICER_GAVE_UP;
        }
      }
      return Pair.of(retval, maxheap);
    }
    catch (ExecutionException | InterruptedException e) {
      // should never happen
      throw new IllegalStateException(e);
    }
  }

  protected void makeHashDirs() throws IOException {
    for (int i = 0; i < 16; ++i) {
      for (int j = 0; j < 16; ++j) {
        final File d = new File(String.format("%s/%1x/%1x%1x", cdir, i, i, j)); //NON-NLS
        FileUtils.forceMkdir(d);
      }
    }
  }

  protected void cleanup() throws IOException {
    FileUtils.forceDelete(cdir);
  }

  /**
   * Slices the tiles.
   *
   * @throws IOException if one occurs
   */
  public void sliceTiles() throws CancellationException, IOException {
    final List<String> multi = new ArrayList<>();
    final List<Pair<String, IOException>> failed =
      new ArrayList<>();

    final Pair<Integer, Integer> s;
    try (DataArchive archive = new DataArchive(aname)) {
      final FileStore tcache = new ImageTileDiskCache(cdir.getAbsolutePath());
      s = findImages(archive, tcache, multi, failed);
    }

    // nothing to do if no images need tiling
    if (multi.isEmpty()) {
      logger.info("No images to tile."); //NON-NLS
      return;
    }

    // ensure that the tile directories exist
    makeHashDirs();

    // Fix the max heap
    final int max_data_mbytes = (4 * s.second) >> 20;

    // This was determined empirically.
    final int maxheap = (int) (1.66 * max_data_mbytes + 150);

    final StateMachineHandler h = createStateMachineHandler(s.first, null);
    h.handleStart();

    // slice, and cleanup on failure
    try {
      // result is (return value, next max heap to try)
      Pair<Integer, Integer> result = Pair.of(0, maxheap);
      do {
        result = runSlicer(multi, result.second, h);
      } while (result.second > 0);

      if (result.first == 0) {
        h.handleSuccess();
      }
      else {
        h.handleFailure();

        final String likely = result.first == 2 ? "Probably" : "Possibly"; //NON-NLS
        throw new IOException(likely + " ran out of memory. Tiling failed with return value == " + result.first);
      }
    }
    catch (CancellationException | IOException e) {
      cleanup();
      throw e;
    }
  }
}
