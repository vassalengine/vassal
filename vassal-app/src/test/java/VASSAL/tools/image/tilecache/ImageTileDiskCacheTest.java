/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.endsWith;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ImageTileDiskCacheTest {

  private static final String IMAGE = "images/map.png";

  @TempDir
  Path root;

  /** Puts an empty file where a tile would be; only its presence is read. */
  private void addTile(String image, int tileX, int tileY, int div) throws IOException {
    final Path p = root.resolve(TileUtils.tileName(image, tileX, tileY, div));
    Files.createDirectories(p.getParent());
    Files.createFile(p);
  }

  private String describe(int tileX, int tileY, int div) {
    return ImageTileDiskCache.describeMissingTile(root.toFile(), IMAGE, tileX, tileY, div);
  }

  private static String subdirOf(String image, int tileX, int tileY, int div) {
    final String tname = TileUtils.tileName(image, tileX, tileY, div);
    return tname.substring(0, tname.lastIndexOf('/'));
  }

  @Test
  public void saysWhichTileOfWhichImageAndWhere() {
    assertThat(describe(1, 0, 2), startsWith(
      "Tile 1,0@1:2 of " + IMAGE + " is missing from " + root.toFile() + ": "
    ));
  }

  @Test
  public void saysWhenTheWholeCacheIsGone() {
    final File gone = root.resolve("gone").toFile();

    assertThat(
      ImageTileDiskCache.describeMissingTile(gone, IMAGE, 1, 0, 2),
      endsWith(": the cache directory does not exist")
    );
  }

  @Test
  public void saysWhenTheSubdirectoryIsGone() {
    assertThat(describe(1, 0, 2), containsString(
      "; subdirectory " + subdirOf(IMAGE, 1, 0, 2) + " does not exist"
    ));
  }

  @Test
  public void countsTheTilesStillBesideTheMissingOne() throws IOException {
    // two files beside where the missing tile would be, whatever they hold
    final Path sub = root.resolve(subdirOf(IMAGE, 1, 0, 2));
    Files.createDirectories(sub);
    Files.createFile(sub.resolve("a"));
    Files.createFile(sub.resolve("b"));

    assertThat(describe(1, 0, 2), containsString(
      "; subdirectory " + subdirOf(IMAGE, 1, 0, 2) + " holds 2 tiles and last changed 2"
    ));
  }

  @Test
  public void saysWhenTheDirectoriesLastChanged() throws IOException {
    Files.createDirectories(root.resolve(subdirOf(IMAGE, 1, 0, 2)));

    // a date and a time, as the log writes its own, for both directories
    final String when = "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}";
    final String d = describe(1, 0, 2);
    assertEquals(true, d.matches(
      ".*: the cache directory last changed " + when +
      "; subdirectory \\S+ holds 0 tiles and last changed " + when + "; .*"
    ), d);
  }

  @Test
  public void listsTheScalesAtWhichTheImageStillHasItsFirstTile() throws IOException {
    // one whole scale gone, the rest intact: what vassalengine/vassal#15413
    // would look like if only the 1:2 tiles had been lost
    addTile(IMAGE, 0, 0, 1);
    addTile(IMAGE, 0, 0, 4);
    addTile(IMAGE, 0, 0, 8);

    assertThat(describe(1, 0, 2), endsWith(
      "; the first tile of this image is present at 1:1, 1:4, 1:8"
    ));
  }

  @Test
  public void saysWhenTheImageHasNoTilesAtAll() throws IOException {
    // another image's tiles do not count
    addTile("images/other.png", 0, 0, 1);

    assertThat(describe(1, 0, 2), endsWith(
      "; the first tile of this image is present at no scale"
    ));
  }

  @Test
  public void aMissingTileIsStillReportedAsBefore() {
    final ImageTileDiskCache cache = new ImageTileDiskCache(root.toString());

    // describing the cache is in addition to the exception, not instead of
    // it, and asking twice describes once but throws both times
    for (int i = 0; i < 2; ++i) {
      final TileNotFoundException e = assertThrows(
        TileNotFoundException.class,
        () -> cache.getTile(IMAGE, 1, 0, 0.5)
      );
      assertThat(e.getMessage(), endsWith("map.png:1,0@0.5"));
      assertThat(e.getMessage(), not(containsString("cache directory")));

      assertThrows(
        TileNotFoundException.class,
        () -> cache.getTileSize(IMAGE, 1, 0, 0.5)
      );
    }
  }
}
