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

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import javax.imageio.ImageIO;

import VASSAL.tools.io.TemporaryFileFactory;
import VASSAL.tools.lang.Reference;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.*;
import static VASSAL.tools.image.AssertImage.*;

@RunWith(JMock.class)
public class FallbackImageTypeConverterTest {

  protected static final String test = "test/VASSAL/tools/image/rainbow.jpg";
  protected static final String tmp = "test/VASSAL/tools/image/img.tmp";

  protected static final TemporaryFileFactory tf = new TemporaryFileFactory() {
    public File create() {
      return new File(tmp);
    }
  };

  protected static BufferedImage src;

  @BeforeClass
  public static void setup() throws IOException {
    src = ImageIO.read(new File(test));
  }

  protected final Mockery context = new JUnit4Mockery();

  @Test
  public void testLoadMemory() throws IOException {
    final Reference<BufferedImage> ref = new Reference<BufferedImage>(src);
    final File tmpFile = new File(tmp);

    BufferedImage dst = null;
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
    final Reference<BufferedImage> ref = new Reference<BufferedImage>(src);
    final File tmpFile = new File(tmp);

    final ImageTypeConverter mock = context.mock(ImageTypeConverter.class);

    context.checking(new Expectations() {
      {
        // this will force a fallback to the file converter
        allowing(mock).convert(with(any(Reference.class)),
                               with(any(int.class)));
        will(throwException(new OutOfMemoryError()));
      }
    });

    BufferedImage dst = null;
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
