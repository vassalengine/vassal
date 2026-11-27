/*
 *
 * Copyright (c) 2026 by the Vassal developers
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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.arrayContaining;
import static org.hamcrest.Matchers.containsString;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;

import VASSAL.tools.DataArchive;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.FileStore;
import VASSAL.tools.lang.Pair;

import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Future;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

public class TilingHandlerTest {
  @TempDir
  Path dir;


  private static final Dimension TILE = new Dimension(256, 256);

  /** Big enough to need more than one tile at 1:1. */
  private static final Dimension BIG = new Dimension(2000, 1500);

  /** Small enough to fit in a single tile, so never worth tiling. */
  private static final Dimension SMALL = new Dimension(50, 50);

  /**
   * A handler whose archive is a fixture: the paths it offers, the size of
   * each (or an IOException), and which ones it considers already tiled.
   */
  private static class FixtureHandler extends TilingHandler {
    private final Map<String, Object> images;
    private final Set<String> fresh;

    FixtureHandler(Map<String, Object> images, Set<String> fresh) {
      this("module.vmod", new File("cache"), images, fresh); //NON-NLS
    }

    FixtureHandler(String aname, File cdir, Map<String, Object> images, Set<String> fresh) {
      super(aname, cdir, TILE, 1024);
      this.images = images;
      this.fresh = fresh;
    }

    @Override
    protected Iterable<String> getImagePaths(DataArchive archive) {
      return images.keySet();
    }

    @Override
    protected Dimension getImageSize(DataArchive archive, String ipath) throws IOException {
      final Object o = images.get(ipath);
      if (o instanceof IOException) {
        throw (IOException) o;
      }
      return (Dimension) o;
    }

    @Override
    protected boolean isFresh(FileArchive archive, FileStore tcache,
                              String ipath, Dimension idim) {
      return fresh.contains(ipath);
    }
  }

  @Test
  public void directoriesAndSvgsAreNotImagesToTile() {
    assertThat(TilingHandler.isTileable("images/map.png"), is(true)); //NON-NLS
    assertThat(TilingHandler.isTileable("images/sub/map.png"), is(true)); //NON-NLS

    // a directory entry is not an image
    assertThat(TilingHandler.isTileable("images/sub/"), is(false)); //NON-NLS

    // an SVG is drawn from its source, never from tiles
    assertThat(TilingHandler.isTileable("images/icon.svg"), is(false)); //NON-NLS

    // Op.load() tests the suffix case-sensitively, so anything else is
    // loaded as a bitmap and is tileable, typo or not
    assertThat(TilingHandler.isTileable("images/icon.SVG"), is(true)); //NON-NLS
  }

  /**
   * An image which cannot be measured cannot be tiled, and will be asked for
   * at render time regardless, so it must be reported rather than dropped.
   * Entries which were never tileable in the first place are not failures.
   */
  @Test
  public void unreadableImagesAreReportedAndNonImagesAreNot() throws IOException {
    final Map<String, Object> images = new LinkedHashMap<>();
    images.put("images/big.png", BIG);                                 //NON-NLS
    images.put("images/already.png", BIG);                             //NON-NLS
    images.put("images/tiny.png", SMALL);                              //NON-NLS
    images.put("images/broken.png", new IOException("unreadable"));    //NON-NLS
    images.put("images/icon.svg", new IOException("not an SVG reader")); //NON-NLS
    images.put("images/sub/", new IOException("a directory"));         //NON-NLS

    final TilingHandler th =
      new FixtureHandler(images, Set.of("images/already.png")); //NON-NLS

    final List<String> multi = new ArrayList<>();
    final List<Pair<String, IOException>> failed = new ArrayList<>();

    th.findImages(mock(DataArchive.class), mock(FileStore.class), multi, failed);

    // only the image which is both tileable and stale needs tiling
    assertThat(multi, contains("images/big.png")); //NON-NLS

    // the unreadable image is reported; the SVG and the directory are not
    assertThat(failed.size(), is(1));
    assertThat(failed.get(0).first, is("images/broken.png")); //NON-NLS
  }

  /**
   * A handler which gets past measuring the images (one big one in a real,
   * empty archive) so that the tile directories are made and the slicer runs.
   */
  private static class SlicingHandler extends FixtureHandler {
    private final IOException cleanupFailure;

    SlicingHandler(String aname, File cdir, IOException cleanupFailure) {
      super(aname, cdir, Map.of("images/big.png", BIG), Set.of()); //NON-NLS
      this.cleanupFailure = cleanupFailure;
    }

    @Override
    protected StateMachineHandler createStateMachineHandler(int tcount, Future<Integer> fut) {
      return new StateMachineHandler() {
        @Override
        public void handleStart() {}
        @Override
        public void handleStartingImageState(String in) {}
        @Override
        public void handleTileWrittenState() {}
        @Override
        public void handleTilingFinishedState() {}
        @Override
        public void handleSuccess() {}
        @Override
        public void handleFailure() {}
      };
    }

    @Override
    protected Pair<Integer, Integer> runSlicer(List<String> multi, int maxheap, StateMachineHandler h) {
      // the slicer failed and there is nothing left to retry
      return Pair.of(2, SLICER_GAVE_UP);
    }

    @Override
    protected void cleanup() throws IOException {
      if (cleanupFailure != null) {
        throw cleanupFailure;
      }
      super.cleanup();
    }
  }

  private File emptyArchive() throws IOException {
    final File f = dir.resolve("module.vmod").toFile(); //NON-NLS
    try (ZipOutputStream zout = new ZipOutputStream(Files.newOutputStream(f.toPath()))) {
      zout.putNextEntry(new ZipEntry("buildFile.xml")); //NON-NLS
      zout.closeEntry();
    }
    return f;
  }

  /**
   * When the slicer fails and the tile directory cannot then be removed, the
   * exception from the slicer is the one to report; the failed cleanup used
   * to replace it.
   */
  @Test
  public void aFailedCleanupDoesNotHideTheSlicerFailure() throws IOException {
    final File cdir = dir.resolve("tiles").toFile(); //NON-NLS
    final IOException locked = new IOException("Cannot delete file: " + cdir); //NON-NLS
    final TilingHandler th = new SlicingHandler(emptyArchive().getPath(), cdir, locked);

    final IOException e = assertThrows(IOException.class, th::sliceTiles);
    assertThat(e.getMessage(), containsString("ran out of memory")); //NON-NLS
    assertThat(e.getSuppressed(), is(arrayContaining(locked)));
  }

  /**
   * Failing to create the tile directories is a problem with the cache
   * folder, not a bug, so it is reported as such.
   */
  @Test
  public void anUnwritableCacheFolderIsReportedAsATileCacheProblem() throws IOException {
    // a plain file where the cache directory should be
    final File blocker = Files.createFile(dir.resolve("tiles")).toFile(); //NON-NLS
    final File cdir = new File(blocker, "0123456789abcdef"); //NON-NLS
    final TilingHandler th = new SlicingHandler(emptyArchive().getPath(), cdir, null);

    final TileCacheException e = assertThrows(TileCacheException.class, th::sliceTiles);
    assertThat(e.getDirectory(), is(cdir));
  }
}
