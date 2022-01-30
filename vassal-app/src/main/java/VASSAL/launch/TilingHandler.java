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

import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.STARTING_IMAGE;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILE_WRITTEN;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILER_READY;
import static VASSAL.tools.image.tilecache.ZipFileImageTilerState.TILING_FINISHED;

import java.awt.Dimension;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import org.apache.commons.io.FileUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
import VASSAL.tools.lang.Pair;
import VASSAL.tools.swing.EDT;
import VASSAL.tools.swing.ProgressDialog;
import VASSAL.tools.swing.Progressor;

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

  private int retried = 0;

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
      // skip images with fresh tiles
      if (isFresh(fa, tcache, ipath)) continue;
      final Dimension idim;
      try {
        idim = getImageSize(archive, ipath);
      }
      catch (IOException e) {
        // skip images we can't read
        failed.add(Pair.of(ipath, e));
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

    void handleStartingImageState(String in);

    void handleTileWrittenState();

    void handleTilingFinishedState();

    void handleSuccess();

    void handleFailure();
  }

  private static class MyStateMachineHandler implements StateMachineHandler {
    private final Future<Integer> fut;
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
    public void handleStartingImageState(String ipath) {
      EDT.execute(() -> {
        pd.setLabel(Resources.getString("TilingHandler.tiling", ipath));
        if (!pd.isVisible()) {
          pd.setVisible(true);
        }
      });
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

  protected void runSlicer(List<String> multi, int tcount, int maxheap) throws CancellationException, IOException {

    // don't exceed the maxheap limit
    maxheap = Math.min(maxheap, maxheap_limit);

    final String[] args = makeSlicerCommandLine(maxheap);

    // set up the process
    final InputStreamPump errP = new InputOutputStreamPump(null, System.err);

    final ProcessWrapper proc = new ProcessLauncher().launch(
      null, null, errP, args
    );

    final StateMachineHandler h = createStateMachineHandler(tcount, proc.future);

    // read state changes from child's stdout
    try (DataInputStream in = new DataInputStream(proc.stdout)) {

      // This code exists because the JVM prints errors to stdout.
      // Nothing should do this in 2022. What a piece of shit.
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      try {
        byte b;
        while (true) {
          b = in.readByte();

          if (b == TILER_READY) {
            break;
          }
          else {
            // Collect whatever garbage the JVM prints
            baos.write(b);
          }
        }
      }
      finally {
        if (baos.size() > 0) {
          logger.error(baos.toString(StandardCharsets.UTF_8));
        }
      }

      // write the image paths to child's stdin, one per line; do this in the
      // background, so that nothing is blocked if the buffer fills up
      new Thread(() -> {
        try (PrintWriter stdin = new PrintWriter(proc.stdin, true, StandardCharsets.UTF_8)) {
          multi.forEach(stdin::println);
        }
      }).start();

      h.handleStart();

      boolean done = false;
      byte type;
      while (!done) {
        type = in.readByte();

        switch (type) {
        case STARTING_IMAGE:
          h.handleStartingImageState(in.readUTF());
          break;

        case TILE_WRITTEN:
          h.handleTileWrittenState();
          break;

        case TILING_FINISHED:
          done = true;
          h.handleTilingFinishedState();
          break;

        default:
          throw new IllegalStateException("bad type: " + type);
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
        h.handleSuccess();
      }
      else {
        h.handleFailure();
        proc.future.cancel(true);

        logger.info("Tiling failed with return value == " + retval);

        if (retried < 2) {
          // The tiler possibly ran out of memory; we can't reliably detect
          //  this, so assume it did. Try again with 50% more max heap.
          logger.info("Tiling possibly ran out of memory. Retrying tiling with 50% more."); //NON-NLS
          ++retried;
          runSlicer(multi, tcount, (int)(maxheap * 1.5));
        }
        else {
          throw new IOException("return value == " + retval);
        }
      }
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

    final int max_data_mbytes = (4 * s.second) >> 20;

    // fix the max heap

    // This was determined empirically.
    final int maxheap = (int) (1.66 * max_data_mbytes + 150);

    // slice, and cleanup on failure
    try {
      runSlicer(multi, s.first, maxheap);
    }
    catch (CancellationException | IOException e) {
      cleanup();
      throw e;
    }
  }
}
