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
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import VASSAL.tools.io.TemporaryFileFactory;
import VASSAL.tools.lang.Reference;

/**
 * Convert a {@link BufferedImage} to a different type by caching image
 * data on disk.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class FileImageTypeConverter implements ImageTypeConverter {

  protected final TemporaryFileFactory tfactory;

  /**
   * Create a converter.
   *
   * @param tfactory the temporary file to use as a cache
   */
  public FileImageTypeConverter(TemporaryFileFactory tfactory) {
    if (tfactory == null) throw new IllegalArgumentException();
    this.tfactory = tfactory;
  }

  /**
   * {@inheritDoc}
   *
   * <b>WARNING: When this method is called, the sole reference to the image
   * must be the one held by <code>ref</code> in order to allow the source
   * image to be garbage collected after the image data is written to disk.</b>
   */
  @Override
  public BufferedImage convert(Reference<BufferedImage> ref, int type)
                                                      throws ImageIOException {
    if (ref == null) throw new IllegalArgumentException();

    // This is why we pass the image via a Reference:
    //
    // So long as the calling method holds no references to the image, we
    // can ensure that our reference to the source image is the only one.
    //
    // This converter doesn't require the source and destination images to
    // exist simultaneously, so when we finish with the source, we can null
    // its reference. Because there are no other references to the source
    // image, this will make it eligible for garbage collection.
    //
    // Hence, we can let the source image be gc'd before we create the
    // destination image, potentially saving a huge amount of RAM.
    //
    BufferedImage src = ref.obj;
    ref.obj = null;

    // we can't create images of TYPE_CUSTOM
    if (type == BufferedImage.TYPE_CUSTOM) throw new IllegalArgumentException();

    final File tmp;
    try {
      tmp = tfactory.create();
    }
    catch (IOException e) {
      throw new ImageIOException("", e);
    }

    try {
      // write the converted image data to a file
      try (OutputStream fout = Files.newOutputStream(tmp.toPath());
           OutputStream gzout = new GZIPOutputStream(fout);
           OutputStream out = new BufferedOutputStream(gzout)) {
        write(src, out);
      }
      catch (IOException e) {
        throw new ImageIOException(tmp, e);
      }

      final int w = src.getWidth();
      final int h = src.getHeight();

      // ensure that src can be gc'd before we create dst
      src = null;

      final BufferedImage dst = new BufferedImage(w, h, type);

      // read the converted image data back
      try (InputStream fin = Files.newInputStream(tmp.toPath());
           InputStream gzin = new GZIPInputStream(fin);
           InputStream in = new BufferedInputStream(gzin)) {
        read(in, dst);
        return dst;
      }
      catch (IOException e) {
        throw new ImageIOException(tmp, e);
      }
    }
    finally {
      // clean up the temporary file
      if (!tmp.delete()) {
        throw new ImageIOException(tmp, "failed to delete");
      }
    }
  }

  protected void write(BufferedImage src, OutputStream out)
                                                           throws IOException {
    final int w = src.getWidth();
    final int h = src.getHeight();

    final ByteBuffer bb = ByteBuffer.allocate(4 * w);
    final int[] row = new int[w];

    for (int y = 0; y < h; ++y) {
      // get the row in ARGB format
      src.getRGB(0, y, w, 1, row, 0, w);

      // copy the row to the byte buffer
      bb.asIntBuffer().put(row);

      // write the row to the stream
      out.write(bb.array());
    }
  }

  protected void read(InputStream in, BufferedImage dst) throws IOException {
    final int w = dst.getWidth();
    final int h = dst.getHeight();

    final byte[] bytes = new byte[4 * w];
    final ByteBuffer bb = ByteBuffer.wrap(bytes);

    final int[] row = new int[w];

    for (int y = 0; y < h; ++y) {
      // read the row from the stream
      in.readNBytes(bytes, 0, bytes.length);

      // convert the bytes to an int[]
      bb.asIntBuffer().get(row);

      // write the row back to the image
      dst.setRGB(0, y, w, 1, row, 0, w);
    }
  }
}
