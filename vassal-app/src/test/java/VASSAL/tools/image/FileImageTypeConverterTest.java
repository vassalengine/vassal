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

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import javax.imageio.ImageIO;

import VASSAL.tools.io.TemporaryFileFactory;
import VASSAL.tools.lang.Reference;

import org.junit.BeforeClass;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static VASSAL.tools.image.AssertImage.*;

public class FileImageTypeConverterTest {

  private static final String test = "src/test/resources/test-images/rainbow.jpg";
  private static final String tmp = "src/test/resources/test-images/img.tmp";

  private static final TemporaryFileFactory tf = () -> new File(tmp);

  /**
   * This class exposes read() and write() for testing.
   */
  private static class FITC extends FileImageTypeConverter {
    public FITC() {
      super(tf);
    }

    public void write(BufferedImage src, OutputStream out) throws IOException {
      super.write(src, out);
    }

    public void read(InputStream in, BufferedImage dst) throws IOException {
      super.read(in, dst);
    }
  }

  private static BufferedImage src;

  @BeforeEach
  public void setup() throws IOException {
    src = ImageIO.read(new File(test));
  }

  @Test
  public void testWriteRead() throws IOException {
    final FITC c = new FITC();

    final ByteArrayOutputStream out = new ByteArrayOutputStream();

    c.write(src, out);

    final BufferedImage dst = new BufferedImage(
      src.getWidth(), src.getHeight(), BufferedImage.TYPE_INT_ARGB
    );

    final ByteArrayInputStream in =
      new ByteArrayInputStream(out.toByteArray());

    c.read(in, dst);

    assertImageContentEquals(src, dst);
  }

  @Test
  public void testLoad() throws IOException {
    final Reference<BufferedImage> ref = new Reference<BufferedImage>(src);
    final File tmpFile = new File(tmp);

    BufferedImage dst = null;
    try {
      final FileImageTypeConverter c = new FileImageTypeConverter(tf);
      dst = c.convert(ref, BufferedImage.TYPE_INT_ARGB);

      assertEquals(BufferedImage.TYPE_INT_ARGB, dst.getType());
      assertImageContentEquals(src, dst);
      assertFalse(tmpFile.exists());
    }
    finally {
      // cleanup
      tmpFile.delete();
    }
  }
}
