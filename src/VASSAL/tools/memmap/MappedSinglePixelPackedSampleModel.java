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

import java.awt.image.DataBuffer;
import java.awt.image.RasterFormatException;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.io.IOException;

/**
 * A {@link SinglePixelPackedSampleModel} suitable for use with
 * {@link MappedWritableRaster}.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class MappedSinglePixelPackedSampleModel
           extends SinglePixelPackedSampleModel {
  public MappedSinglePixelPackedSampleModel(int dataType,
                                            int w, int h,
                                            int bitMasks[]) {
    this(dataType, w, h, w, bitMasks);
  }

  public MappedSinglePixelPackedSampleModel(int dataType,
                                            int w, int h,
                                            int scanlineStride,
                                            int bitMasks[]) {
    super(dataType, w, h, scanlineStride, bitMasks);
  }

  public int getNumDataElements() {
    return 1;
  }

  @Override
  public SampleModel createCompatibleSampleModel(int w, int h) {
    return new MappedSinglePixelPackedSampleModel(dataType,
                                                  w, h,
                                                  getBitMasks());
  }

  private long getBufferSize() {
    return getScanlineStride() * (height-1) + width;
  }

// Removed because we don't want to create new mapped buffers except
// through the constructor.
/*
  @Override
  public DataBuffer createDataBuffer() {
    DataBuffer dataBuffer = null;
    final int size = (int) getBufferSize();
    switch (dataType) {
    case DataBuffer.TYPE_INT:
      try {
        dataBuffer = new MappedDataBufferInt(size);
      }
      catch (IOException e) {
// FIXME: throw more appropriate exception
        throw new IllegalArgumentException();
      }
      break;
    }
    return dataBuffer;  
  }
*/

  @Override
  public SampleModel createSubsetSampleModel(int bands[]) {
    if (bands.length > numBands)
      throw new RasterFormatException("There are only " + numBands + " bands");
    final int newBitMasks[] = new int[bands.length];
    final int bitMasks[] = getBitMasks();
    for (int i = 0; i < bands.length; i++)
      newBitMasks[i] = bitMasks[bands[i]];

    return new MappedSinglePixelPackedSampleModel(this.dataType,
                                                  width, height,
                                                  getScanlineStride(),
                                                  newBitMasks);
  }
}
