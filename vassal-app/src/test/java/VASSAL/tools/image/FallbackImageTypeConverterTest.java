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

import VASSAL.tools.io.TemporaryFileFactory;
import VASSAL.tools.lang.Reference;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static VASSAL.tools.image.AssertImage.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

public class FallbackImageTypeConverterTest {

  private static final String test = "src/test/resources/test-images/rainbow.jpg";
  private static final String tmp = "src/test/resources/test-images/img.tmp";

  private static final TemporaryFileFactory tf = () -> new File(tmp);

  private static BufferedImage src;

  @BeforeEach
  public void setup() throws IOException {
    src = ImageIO.read(new File(test));
  }

  @Test
  public void testLoadMemory() throws IOException {
    final Reference<BufferedImage> ref = new Reference<>(src);
    final File tmpFile = new File(tmp);

    BufferedImage dst;
    try {
      final FallbackImageTypeConverter c = new FallbackImageTypeConverter(tf);
      dst = c.convert(ref, BufferedImage.TYPE_INT_ARGB_PRE);

      assertEquals(BufferedImage.TYPE_INT_ARGB_PRE, dst.getType());
      assertImageContentEquals(src, dst);
      assertFalse(tmpFile.exists());
    }
    finally {
      // cleanup
      tmpFile.delete();
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testLoadFile() throws IOException {
    final Reference<BufferedImage> ref = new Reference<>(src);
    final File tmpFile = new File(tmp);

    final ImageTypeConverter mock = mock(ImageTypeConverter.class);

    // this will force a fallback to the file converter
    when(mock.convert(any(Reference.class), any(int.class))).thenThrow(new OutOfMemoryError());

    BufferedImage dst;
    try {
      final FallbackImageTypeConverter c = new FallbackImageTypeConverter(
        tf, mock, new FileImageTypeConverter(tf)
      );
      dst = c.convert(ref, BufferedImage.TYPE_INT_ARGB_PRE);

      assertEquals(BufferedImage.TYPE_INT_ARGB_PRE, dst.getType());
      assertImageContentEquals(src, dst);
      assertFalse(tmpFile.exists());
    }
    finally {
      // cleanup
      tmpFile.delete();
    }
  }

}
