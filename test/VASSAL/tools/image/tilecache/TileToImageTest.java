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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import VASSAL.tools.io.IOUtils;

import org.junit.After;
import org.junit.Test;

import static org.junit.Assert.*;

public class TileToImageTest {
  protected static final String dir = "test/VASSAL/tools/image/tilecache";

  @Test
  public void testTileToImage() throws IOException {

    final String tile = dir + "/in.tile";
    final String out_actual = dir + "/out.png";
    final String out_expected = dir + "/in.png";

    TileToImage.main(new String[] { tile, out_actual });

    final FileInputStream expected = new FileInputStream(out_expected);
    final FileInputStream actual = new FileInputStream(out_actual);
    assertTrue(IOUtils.contentEquals(expected, actual));
  }

  @After
  public void cleanup() {
    new File(dir + "/out.png").delete();
  }
}
