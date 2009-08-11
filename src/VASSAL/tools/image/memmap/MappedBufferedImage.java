/*
 * $Id$
 *
 * Copyright (c) 2007-2009 by Joel Uckelman
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

package VASSAL.tools.image.memmap;

import java.awt.Point;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.ComponentSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.image.SampleModel;
import java.awt.image.WritableRaster;
import java.io.IOException;

/**
 * A {@link BufferedImage} backed by a memory-mapped file.
 * <code>MappedBufferedImage</code>s are useful for storing images
 * on disk which are too large to keep in RAM long-term but for which
 * decoding them at each access would be too slow. The image data
 * is held by a {@link MappedWritableRaster}.
 *
 * Note: Some Java implementations (e.g., on Mac OS X) use private
 * system-specific classes for image rendering, and frequently such code
 * makes undocumented assumptions about how <code>BufferedImage</code>,
 * {@link WritableRaster}, and {@link DataBuffer} have been subclassed.
 * In order to prevent {@link ClassCastException}s, we have no choice but
 * to permit <code>MappedBufferedImage</code>s to be <code>TYPE_CUSTOM</code>,
 * even though this makes handling them more complex for us. Note also that
 * we do not explicitly set the type here; it is set for us by one of the
 * <code>BufferedImage</code> constructors.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @see MappedWritableRaster
 */
public class MappedBufferedImage extends BufferedImage {
  public MappedBufferedImage(int w, int h, int type) {
    this(createMappedBufferedImage(w, h, type));
  }

  public MappedBufferedImage(ColorModel cm, SampleModel sm) {
    super(
      cm,
      MappedWritableRaster.createWritableRaster(
        sm,
        createDataBuffer(sm),
        new Point(0,0)
      ),
      cm.isAlphaPremultiplied(),
      null
    );
  }

  private MappedBufferedImage(Object[] a) {
    super(
      (ColorModel) a[0],
      (WritableRaster) a[1],
      ((ColorModel) a[0]).isAlphaPremultiplied(),
      null
    );
  } 

  private static Object[] createMappedBufferedImage(int w, int h, int type) {
    final ColorModel cm = createColorModel(type);
    final SampleModel sm = createSampleModel(cm, w, h, type);
    final DataBuffer db = createDataBuffer(sm);
    final WritableRaster wr =
      MappedWritableRaster.createWritableRaster(sm, db, new Point(0,0));

    return new Object[] {cm, wr};
  }

  private static ColorModel createColorModel(int type) {
    switch (type) {
    case TYPE_INT_RGB:
      return new DirectColorModel(
        24,
        0x00ff0000, // R
        0x0000ff00, // G
        0x000000ff, // B
        0x00000000  // A
      );
    case TYPE_INT_ARGB:
      return new DirectColorModel(
        32,
        0x00ff0000, // R
        0x0000ff00, // G
        0x000000ff, // B
        0xff000000  // A
      );
    case TYPE_INT_ARGB_PRE:
      return new DirectColorModel(
        ColorSpace.getInstance(ColorSpace.CS_sRGB),
        32,
        0x00ff0000, // R
        0x0000ff00, // G
        0x000000ff, // B
        0xff000000, // A
        true,
        DataBuffer.TYPE_INT
      );
    case TYPE_INT_BGR:
      return new DirectColorModel(
        24,
        0x000000ff, // R
        0x0000ff00, // G
        0x00ff0000  // B
      );
    case TYPE_3BYTE_BGR:
      return new ComponentColorModel(
        ColorSpace.getInstance(ColorSpace.CS_sRGB),
        new int[] {8, 8, 8},
        false,
        false,
        OPAQUE,
        DataBuffer.TYPE_BYTE
      );
    case TYPE_4BYTE_ABGR:
      return new ComponentColorModel(
        ColorSpace.getInstance(ColorSpace.CS_sRGB),
        new int[] {8, 8, 8, 8},
        true,
        false,
        TRANSLUCENT,
        DataBuffer.TYPE_BYTE
      );
    case TYPE_4BYTE_ABGR_PRE:
      return new ComponentColorModel(
        ColorSpace.getInstance(ColorSpace.CS_sRGB),
        new int[] {8, 8, 8, 8},
        true,
        true,
        TRANSLUCENT,
        DataBuffer.TYPE_BYTE
      );
    case TYPE_BYTE_GRAY:
      return new ComponentColorModel(
        ColorSpace.getInstance(ColorSpace.CS_GRAY),
        new int[] {8},
        false,
        true,
        OPAQUE,
        DataBuffer.TYPE_BYTE
      );
    case TYPE_USHORT_GRAY:
      return new ComponentColorModel(
        ColorSpace.getInstance(ColorSpace.CS_GRAY),
        new int[] {16},
        false,
        true,
        OPAQUE,
        DataBuffer.TYPE_USHORT
      );
    case TYPE_BYTE_BINARY:
      final byte[] c = { (byte)0x00, (byte)0xff };
      return new IndexColorModel(1, 2, c, c, c);
    case TYPE_BYTE_INDEXED:
      final int[] cmap = new int[256];
      int i = 0;
      for (int r = 0; r < 256; r += 51) {
        for (int g = 0; g < 256; g += 51) {
          for (int b = 0; b < 256; b += 51) {
            cmap[i++] = (r << 16) | (g << 8) | b;
          }
        }
      }
      
      // And populate the rest of the cmap with gray values
      final int grayIncr = 256/(256-i);

      // The gray ramp will be between 18 and 252
      int gray = grayIncr*3;
      for ( ; i < 256; i++) {
        cmap[i] = (gray << 16) | (gray << 8) | gray;
        gray += grayIncr;
      }

      return new IndexColorModel(
        8,
        256,
        cmap,
        0,
        false,
        -1,
        DataBuffer.TYPE_BYTE
      );
    case TYPE_USHORT_565_RGB:
      return new DirectColorModel(
        16,
        0xf800, // R
        0x07E0, // G
        0x001F  // B
      );
    case TYPE_USHORT_555_RGB:
      return new DirectColorModel(
        15,
        0x7C00, // R 
        0x03E0, // G
        0x001F  // B
      );
    default:
      throw new IllegalArgumentException("Unsupported image type " + type);
    }
  }

  private static SampleModel createSampleModel(ColorModel cm,
                                               int w, int h, int type) {
    switch (type) {
    case TYPE_INT_RGB:
    case TYPE_INT_ARGB:
    case TYPE_INT_ARGB_PRE:
    case TYPE_INT_BGR:
      return cm.createCompatibleSampleModel(w, h);
    case TYPE_3BYTE_BGR:
      return new PixelInterleavedSampleModel(
        DataBuffer.TYPE_BYTE,
        w,
        h,
        3,
        3*w,
        new int[] {2, 1, 0}
      );
    case TYPE_4BYTE_ABGR:
      return new PixelInterleavedSampleModel(
        DataBuffer.TYPE_BYTE,
        w,
        h,
        4,
        4*w,
        new int[] {3, 2, 1, 0}
      );
    case TYPE_4BYTE_ABGR_PRE:
      return new PixelInterleavedSampleModel(
        DataBuffer.TYPE_BYTE,
        w,
        h,
        4,
        4*w,
        new int[] {3, 2, 1, 0}
      );
    case TYPE_BYTE_GRAY:
    case TYPE_USHORT_GRAY:
    case TYPE_BYTE_BINARY:
    case TYPE_BYTE_INDEXED:
    case TYPE_USHORT_565_RGB:
    case TYPE_USHORT_555_RGB:
      return cm.createCompatibleSampleModel(w, h);
    default:
      throw new IllegalArgumentException("Unsupported image type " + type);
    }
  }

  private static DataBuffer createDataBuffer(SampleModel sm) {
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
    try {
      final int type = sm.getDataType();
      switch (type) {
      case DataBuffer.TYPE_BYTE:
        return new MappedDataBufferByte(banks, size);
      case DataBuffer.TYPE_USHORT:
        return new MappedDataBufferUShort(banks, size);
      case DataBuffer.TYPE_INT:
        return new MappedDataBufferInt(banks, size);
      case DataBuffer.TYPE_SHORT:
      case DataBuffer.TYPE_FLOAT:
      case DataBuffer.TYPE_DOUBLE:
      default:
        throw new IllegalArgumentException("Unsupported image type " + type);
      }
    }
    catch (IOException e) {
      throw (OutOfMemoryError) (new OutOfMemoryError().initCause(e));
    }
  }
}
