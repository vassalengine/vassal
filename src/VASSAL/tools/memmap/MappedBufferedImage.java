/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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

package VASSAL.tools.memmap;

import java.awt.Point;
import java.awt.image.ColorModel;
import java.awt.image.ComponentSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;
import java.awt.image.BufferedImage;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.WritableRaster;
import java.io.IOException;

/**
 * A {@link BufferedImage} backed by a memory-mapped file.
 * <code>MappedBufferedImage</code>s are useful for storing images
 * on disk which are too large to keep in RAM long-term but for which
 * decoding them at each access would be too slow. The image data
 * is held by a {@link MappedWritableRaster}.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @see MappedWritableRaster
 */
public class MappedBufferedImage extends BufferedImage {
  private MappedBufferedImage(ColorModel cm, WritableRaster raster) {
    super(cm, raster, cm.isAlphaPremultiplied(), null);
  }

  public static BufferedImage createMemoryMappedImage(ColorModel cm,
                                                      SampleModel sm)
                                                      throws IOException {
    // determine how many banks the DataBuffer should have
    int maxBank = 0;
    if (sm instanceof ComponentSampleModel) {
      final int numBands = sm.getNumBands();
      if (numBands != 1) {
        final int[] bankIndices = ((ComponentSampleModel) sm).getBankIndices();
        for (int i = 0; i < numBands; ++i) {
          if (bankIndices[i]  > maxBank) maxBank = bankIndices[i];
        }
      }
    }
  
    final int w = sm.getWidth();
    final int h = sm.getHeight();

    final int banks = maxBank + 1;
    final int size = w*h*sm.getNumDataElements()/banks;        

    // create the mapped DataBuffer
    DataBuffer db = null;
    switch (sm.getDataType()) {
    case DataBuffer.TYPE_BYTE:
      db = new MappedDataBufferByte(banks, size);
      break;
    case DataBuffer.TYPE_USHORT:
      db = new MappedDataBufferUShort(banks, size);
      break;
    case DataBuffer.TYPE_INT:
      db = new MappedDataBufferInt(banks, size);
      break;
    case DataBuffer.TYPE_SHORT:
    case DataBuffer.TYPE_FLOAT:
    case DataBuffer.TYPE_DOUBLE:
    default:
      assert false;
    }

    final WritableRaster raster =
      MappedWritableRaster.createWritableRaster(sm, db, new Point(0,0)); 
    
    return new MappedBufferedImage(cm ,raster);
  }

  public static BufferedImage createIntARGBMemoryMappedImage(int w, int h)
      throws IOException {
    return createMemoryMappedImage(
      new DirectColorModel(
        32,
        0x00ff0000,
        0x0000ff00,
        0x000000ff,
        0xff000000),
      new SinglePixelPackedSampleModel(
        DataBuffer.TYPE_INT, w, h,
        new int[] { 0x00ff0000, 0x0000ff00, 0x000000ff, 0xff000000 })
    );
  }
}
