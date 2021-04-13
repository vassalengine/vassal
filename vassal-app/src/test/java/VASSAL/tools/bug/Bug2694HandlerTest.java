/*
 *
 * Copyright (c) 2011 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.bug;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class Bug2694HandlerTest {

  protected static final UnsatisfiedLinkError e = new UnsatisfiedLinkError("sun.awt.image.ImageRepresentation.setBytePixels(IIII[BIILsun/awt/image/ByteComponentRaster;I)V");

  static {
    e.setStackTrace(new StackTraceElement[] {
      new StackTraceElement("sun.awt.image.ImageRepresentation", "setBytePixels", null, -2),
      new StackTraceElement("sun.awt.image.ImageRepresentation", "setPixels", null, -1),
      new StackTraceElement("sun.awt.image.ImageDecoder", "setPixels", null, -1),
      new StackTraceElement("sun.awt.image.GifImageDecoder", "sendPixels", null, -1),
      new StackTraceElement("sun.awt.image.GifImageDecoder", "parseImage", null, -2),
      new StackTraceElement("sun.awt.image.GifImageDecoder", "readImage", null, -1),
      new StackTraceElement("sun.awt.image.GifImageDecoder", "produceImage", null, -1),
      new StackTraceElement("sun.awt.image.InputStreamImageSource", "doFetch", null, -1),
      new StackTraceElement("sun.awt.image.ImageFetcher", "fetchLoop", null, -1),
      new StackTraceElement("sun.awt.image.ImageFetcher", "run", null, -1)
    });
  }

  @Test
  public void testAcceptTrue() {
    final BugHandler bh = new Bug2694Handler();
    assertTrue(bh.accept(e));
  }

  @Test
  public void testAcceptFalse() {
    final BugHandler bh = new Bug2694Handler();
    assertFalse(bh.accept(new Exception()));
  }

  public static void main(String[] args) {
    final BugHandler bh = new Bug2694Handler();
    if (bh.accept(e)) bh.handle(e);
  }
}
