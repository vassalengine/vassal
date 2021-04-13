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

package VASSAL.tools.image;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static VASSAL.tools.image.AssertImage.*;

public class ImageIOImageLoaderTest {
  private static final String jpg = "src/test/resources/test-images/rainbow.jpg";

  private static BufferedImage src;

  @BeforeEach
  public void setup() throws IOException {
    src = ImageIO.read(new File(jpg));
  }

  private BufferedImage read(ImageLoader loader, String file)
                                                           throws IOException {
    try (InputStream in = Files.newInputStream(Path.of(file))) {
      final BufferedImage img = loader.load(
        file, in, BufferedImage.TYPE_INT_RGB,
        BufferedImage.TYPE_INT_ARGB, false
      );
      return img;
    }
  }

  @Test
  public void testLoadOk() throws IOException {
    final ImageTypeConverter mconv = new MemoryImageTypeConverter();
    final ImageIOImageLoader loader = new ImageIOImageLoader(mconv);

    final BufferedImage actual = read(loader, jpg);

    assertEquals(BufferedImage.TYPE_INT_RGB, actual.getType());
    assertImageContentEquals(src, actual);
  }

  @Test
  public void testLoadType2tRNSBug() throws IOException {
    final String efile = "src/test/resources/test-images/non-type2-tRNS.png";
    final String afile = "src/test/resources/test-images/type2-tRNS.png";

    final ImageTypeConverter mconv = new MemoryImageTypeConverter();
    final ImageIOImageLoader loader = new ImageIOImageLoader(mconv);

    final BufferedImage expected = ImageIO.read(new File(efile));
    final BufferedImage actual = read(loader, afile);

    assertEquals(BufferedImage.TYPE_INT_ARGB, actual.getType());

    // We check that:
    // (1) the images have the same fully-transparent pixels, and
    // (2) all other pixels are identical
    final int w = expected.getWidth();
    final int h = expected.getHeight();
    for (int x = 0; x < w; ++x) {
      for (int y = 0; y < h; ++y) {
        final int ep = expected.getRGB(x, y);
        final int ap = actual.getRGB(x, y);

        if ((ep & 0xff000000) == 0 && (ap & 0xff000000) == 0) {
          // color components of fully-transparent pixels don't matter
          continue;
        }

        assertEquals(ep, ap);
      }
    }
  }

  private Dimension size(ImageLoader loader, String file) throws IOException {
    try (InputStream in = Files.newInputStream(Path.of(file))) {
      final Dimension d = loader.size(file, in);
      return d;
    }
  }

  @Test
  public void testSizeOk() throws IOException {
    final ImageTypeConverter mconv = new MemoryImageTypeConverter();
    final ImageIOImageLoader loader = new ImageIOImageLoader(mconv);

    final Dimension ed = new Dimension(src.getWidth(), src.getHeight());
    final Dimension ad = size(loader, jpg);

    assertEquals(ed, ad);
  }

  @Test
  public void testSizeUnrecognized() {
    final String junk = "src/test/resources/test-images/NotAnImageForImageIOImageLoaderTest.txt";

    final ImageTypeConverter mconv = new MemoryImageTypeConverter();
    final ImageIOImageLoader loader = new ImageIOImageLoader(mconv);

    assertThrows(UnrecognizedImageTypeException.class, () -> size(loader, junk));
  }
}
