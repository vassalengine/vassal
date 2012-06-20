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

package VASSAL.tools.image;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import javax.imageio.ImageIO;

import VASSAL.tools.io.IOUtils;

import org.apache.commons.lang.SystemUtils;

import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;
import static VASSAL.tools.image.AssertImage.*;

public class ImageIOImageLoaderTest {
  protected static final String jpg = "test/VASSAL/tools/image/rainbow.jpg";

  protected static BufferedImage src;

  @BeforeClass
  public static void setup() throws IOException {
    src = ImageIO.read(new File(jpg));
  }

  protected BufferedImage read(ImageLoader loader, String file)
                                                           throws IOException {
    FileInputStream in = null;
    try {
      in = new FileInputStream(file);
      final BufferedImage img = loader.load(
        file, in, BufferedImage.TYPE_INT_RGB,
        BufferedImage.TYPE_INT_ARGB, false
      );
      in.close();
      return img;
    }
    finally {
      IOUtils.closeQuietly(in);
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
    final String efile = "test/VASSAL/tools/image/non-type2-tRNS.png";
    final String afile = "test/VASSAL/tools/image/type2-tRNS.png";

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

  @Test
  public void testLoad_iTXTBug() throws IOException {
    final String efile = "test/VASSAL/tools/image/no-iTXt.png";
    final String afile = "test/VASSAL/tools/image/iTXt.png";

    final ImageTypeConverter mconv = new MemoryImageTypeConverter();
    final ImageIOImageLoader loader = new ImageIOImageLoader(mconv);

    final BufferedImage expected = read(loader, efile);
    final BufferedImage actual = read(loader, afile);

    assertEquals(BufferedImage.TYPE_INT_ARGB, actual.getType());
    assertImageContentEquals(expected, actual);
  }

  @Test(expected=BrokenImageException.class)
  public void testLoadRGB_JPEG_NonRGB_Color_Profile() throws IOException {
    final String afile = "test/VASSAL/tools/image/WatervilleDuelpark.jpg";

    final ImageTypeConverter mconv = new MemoryImageTypeConverter();
    final ImageIOImageLoader loader = new ImageIOImageLoader(mconv);

    final BufferedImage actual = read(loader, afile);
  }

  @Test(expected=BrokenImageException.class)
  public void testLoadLCMS_Error() throws IOException {
    // this appears to be fixed in Java >= 1.7
    if (SystemUtils.isJavaVersionAtLeast(1.7f)) {
      throw new BrokenImageException("bogus");
    }

    final String afile = "test/VASSAL/tools/image/09.jpg";

    final ImageTypeConverter mconv = new MemoryImageTypeConverter();
    final ImageIOImageLoader loader = new ImageIOImageLoader(mconv);

    final BufferedImage actual = read(loader, afile);
  }

  protected Dimension size(ImageLoader loader, String file) throws IOException {
    FileInputStream in = null;
    try {
      in = new FileInputStream(file);
      final Dimension d = loader.size(file, in);
      in.close();
      return d;
    }
    finally {
      IOUtils.closeQuietly(in);
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

  @Test(expected=UnrecognizedImageTypeException.class)
  public void testSizeUnrecognized() throws IOException {
    final String junk = "test/VASSAL/tools/image/ImageIOImageLoaderTest.java";

    final ImageTypeConverter mconv = new MemoryImageTypeConverter();
    final ImageIOImageLoader loader = new ImageIOImageLoader(mconv);

    final Dimension ad = size(loader, junk);
  }

  @Test(expected=BrokenImageException.class)
  public void testSizeLCMS_Error() throws IOException {
    // this appears to be fixed in Java >= 1.7
    if (SystemUtils.isJavaVersionAtLeast(1.7f)) {
      throw new BrokenImageException("bogus");
    }

    final String afile = "test/VASSAL/tools/image/09.jpg";

    final ImageTypeConverter mconv = new MemoryImageTypeConverter();
    final ImageIOImageLoader loader = new ImageIOImageLoader(mconv);

    final Dimension ad = size(loader, afile);
  }
}
