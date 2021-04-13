/*
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

import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.commons.io.IOUtils;

import org.junit.After;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class TileToImageTest {
  private static final String in_dir = "src/test/resources/test-images";
  private static final String out_dir = "target/test-classes";

  @Test
  public void testTileToImage() throws IOException {

    final String tile = in_dir + "/in.tile";
    final String out_actual = out_dir + "/out.png";
    final String out_expected = in_dir + "/in.png";

    TileToImage.main(new String[] { tile, out_actual });

    try (InputStream expected = Files.newInputStream(Path.of(out_expected));
         InputStream actual = Files.newInputStream(Path.of(out_actual))) {
      assertTrue(IOUtils.contentEquals(expected, actual));
    }
  }

  @After
  public void cleanup() {
    new File(out_dir + "/out.png").delete();
  }
}
