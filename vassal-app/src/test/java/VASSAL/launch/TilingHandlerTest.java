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
import static org.mockito.Mockito.mock;

import VASSAL.tools.DataArchive;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.FileStore;
import VASSAL.tools.lang.Pair;

import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;

public class TilingHandlerTest {

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
      super("module.vmod", new File("cache"), TILE, 1024); //NON-NLS
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
}
