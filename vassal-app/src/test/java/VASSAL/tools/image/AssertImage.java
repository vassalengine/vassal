/*
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

import org.junit.jupiter.api.Disabled;

import static org.junit.jupiter.api.Assertions.*;

/**
 * A set of assertion methods for writing tests involving images.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
@Disabled
public class AssertImage {
  public static void assertImageEquals(BufferedImage expected,
                                       BufferedImage actual) {
    assertEquals(expected.getType(), actual.getType());
    assertImageContentEquals(expected, actual);
  }

  public static void assertImageContentEquals(BufferedImage expected,
                                              BufferedImage actual) {
    assertEquals(expected.getWidth(),  actual.getWidth());
    assertEquals(expected.getHeight(), actual.getHeight());

    final int w = expected.getWidth();
    final int h = expected.getHeight();

    assertArrayEquals(expected.getRGB(0, 0, w, h, null, 0, w),
                        actual.getRGB(0, 0, w, h, null, 0, w));
  }
}
