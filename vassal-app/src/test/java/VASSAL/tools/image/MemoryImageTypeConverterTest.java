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
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;

import VASSAL.tools.lang.Reference;

import org.junit.jupiter.api.Test;

import static VASSAL.tools.image.AssertImage.*;

public class MemoryImageTypeConverterTest {

  private static final String test = "src/test/resources/test-images/rainbow.jpg";

  @Test
  public void testConvert() throws IOException {
    final BufferedImage src = ImageIO.read(new File(test));
    final ImageTypeConverter c = new MemoryImageTypeConverter();
    final Reference<BufferedImage> ref = new Reference<BufferedImage>(src);
    final BufferedImage dst = c.convert(ref, BufferedImage.TYPE_INT_ARGB);
    assertImageContentEquals(src, dst);
  }
}
