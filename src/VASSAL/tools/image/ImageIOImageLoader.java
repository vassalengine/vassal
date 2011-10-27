/*
 * $Id$
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
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

import java.awt.Dimension;
import java.awt.color.CMMException;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.MemoryCacheImageInputStream;

import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.RereadableInputStream;
import VASSAL.tools.lang.Reference;

/**
 * An image loader which wraps {@link ImageIO}.
 *
 * This class handles the assorted problems with various versions of
 * {@link ImageIO}, ensuring that we can reliably load image files to
 * {@link BufferedImages} with a predictable type.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class ImageIOImageLoader implements ImageLoader {

  protected final ImageTypeConverter tconv;

  /**
   * Create an image loader.
   *
   * @param tconv the <code>ImageTypeConverter</code> to use for type
   * conversions
   */
  public ImageIOImageLoader(ImageTypeConverter tconv) {
    this.tconv = tconv;
  }

  // Used to indicate whether this version of Java has the PNG iTXt bug.
  // This can be removed once we no longer support Java 1.5.
  protected static final boolean iTXtBug;

  static {
    final String jvmver = System.getProperty("java.version");
    iTXtBug = jvmver == null || jvmver.startsWith("1.5");
  }

  protected static final Set<Integer> skip_iTXt =
    Collections.singleton(PNGDecoder.iTXt);

  /**
   * Loads an image.
   *
   * @param name the image name
   * @param in the input stream
   * @param typeIfOpaque the requested image type for opaque images
   * @param typeIfTransparent the requested image type for transparent images
   * @param managed <code>true</code> if a managed image should be returned
   * @return the image
   *
   * @throws BrokenImageException if the image is faulty
   * @throws UnrecognizedImageTypeException if the image type is not recognized
   * @throws ImageIOException if reading the image goes wrong
   */
  public BufferedImage load(
    String name,
    InputStream in,
    int typeIfOpaque,
    int typeIfTransparent,
    boolean managed
  ) throws ImageIOException {
    //
    // ImageIO fails on the following types of images:
    //
    // Sun Bug 6788458: 8-bit/channel color type 2 (RGB) PNGs with tRNS chunks
    // Sun Bug 6541476: PNGs with iTXt chunks on Java 1.5
    // Sun Bug 6444360: JPEGs with corrupt color profiles
    // Sun Bug 6404011: JPEGs with corrupt color profiles on Java 1.5
    //
    // http://bugs.sun.com/view_bug.do?bug_id=6788458
    // http://bugs.sun.com/view_bug.do?bug_id=6541476
    // http://bugs.sun.com/view_bug.do?bug_id=6444360
    // http://bugs.sun.com/view_bug.do?bug_id=6404011
    //
    // Someday, when both ImageIO is fixed and everyone's JRE contains
    // that fix, we can do this the simple way.
    //

    boolean fix_tRNS = false;
    int tRNS = 0x00000000;

    BufferedImage img = null;
    RereadableInputStream rin = null;
    try {
      rin = new RereadableInputStream(in);
      rin.mark(512);

      DataInputStream din = new DataInputStream(rin);
      PNGDecoder.Chunk ch;

      // Is this a PNG?
      if (PNGDecoder.decodeSignature(din)) {
        // The PNG chunks refered to here are defined in the PNG
        // standard, found at http://www.w3.org/TR/PNG/
        ch = PNGDecoder.decodeChunk(din);

        // Sanity check: This is not a PNG if IHDR is not the first chunk.
        if (ch.type == PNGDecoder.IHDR) {
          // At present, ImageIO does not honor the tRNS chunk in 8-bit color
          // type 2 (RGB) PNGs. This is not a bug per se, as the PNG standard
          // the does not require compliant decoders to use ancillary chunks.
          // However, every other PNG decoder we can find *does* honor the
          // tRNS chunk for this type of image, and so the appearance for
          // users is that VASSAL is broken when their 8-bit RGB PNGs don't
          // show the correct transparency.

          // We check for type-2 8-bit PNGs with tRNS chunks.
          if (ch.data[8] == 8 && ch.data[9] == 2) {
            // This is an 8-bit-per-channel Truecolor image; we must check
            // whether there is a tRNS chunk, and if so, record the color
            // so that we can manually set transparency later.
            //
            // IHDR is required to be first, and tRNS is required to appear
            // before the first IDAT chunk; therefore, if we find an IDAT
            // we're done.

            DONE: for (;;) {
              ch = PNGDecoder.decodeChunk(din);

              switch (ch.type) {
              case PNGDecoder.tRNS: fix_tRNS = true;  break DONE;
              case PNGDecoder.IDAT: fix_tRNS = false; break DONE;
              default:
              }
            }

            if (fix_tRNS) {
              if (ch.data.length != 6) {
                // There is at least one piece of software (SplitImage) which
                // writes tRNS chunks for type 2 images which are only 3 bytes
                // long, and because this kind of thing is used by module
                // designers for slicing up scans of countersheets, we can
                // expect to see such crap from time to time.
                throw new BrokenImageException(name, "bad tRNS chunk length");
              }

              //
              // tRNS chunk: PNG Standard, 11.3.2.1
              //
              // tRNS data is stored as three 2-byte samples, but the high
              // byte of each sample is empty because we are dealing with
              // 8-bit-per-channel images.
              tRNS = 0xff000000 |
                     ((ch.data[1] & 0xff) << 16) |
                     ((ch.data[3] & 0xff) <<  8) |
                      (ch.data[5] & 0xff);
            }
          }

          if (iTXtBug) {
            // Filter out iTXt chunks on JVMs with the iTXt bug.
            rin.reset();
            rin = new RereadableInputStream(
              new PNGChunkSkipInputStream(skip_iTXt, rin));
            rin.mark(1);
          }
        }
      }

      // Load the image
      rin.reset();
      img = wrapImageIO(name, rin, readImage);
      rin.close();
    }
    catch (ImageIOException e) {
      // Don't wrap ImageIOExceptions.
      throw e;
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
    finally {
      IOUtils.closeQuietly(rin);
    }

    final int type =
      img.getTransparency() == BufferedImage.OPAQUE && !fix_tRNS
      ? typeIfOpaque : typeIfTransparent;

    final Reference<BufferedImage> ref = new Reference<BufferedImage>(img);

    // Fix up transparency in type 2 Truecolor images.
    if (fix_tRNS) {
      img = null;
      img = fix_tRNS(ref, tRNS, type);
      ref.obj = img;
    }

    // We convert the image in two cases:
    // 1) the image is not yet the requested type, or
    // 2) a managed image was requested, but the image
    //    was unmanaged by the transparency fix.
    if (img.getType() != type || (fix_tRNS && managed)) {
      img = null;
      img = tconv.convert(ref, type);
    }

    return img;
  }

  protected static interface Wrapper<T> {
    T run(String name, InputStream in) throws IOException;
  }

  protected <T> T wrapImageIO(String name, InputStream in, Wrapper<T> w)
                                                      throws ImageIOException {
    try {
      return w.run(name, in);
    }
    catch (ArrayIndexOutOfBoundsException e) {
      // Note: ImageIO can throw an ArrayIndexOutOfBoundsException for
      // some corrupt JPEGs. This problem is noted in Sun Bug 6351707,
      //
      // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6351707
      //
      throw new BrokenImageException(name, e);
    }
    catch (CMMException e) {
      // Note: ImageIO can throw a CMMException for JPEGs which have
      // broken color profiles. This problem is noted in Sun Bugs 6444360
      // and 6839133.
      //
      // http://bugs.sun.com/view_bug.do?bug_id=6444360
      // http://bugs.sun.com/view_bug.do?bug_id=6839133
      //
      throw new BrokenImageException(name, e);
    }
    catch (IllegalArgumentException e) {
      // Note: ImageIO can throw IllegalArgumentExceptions for certain
      // kinds of broken images, e.g., JPEGs which are in the RGB color
      // space but have non-RGB color profiles (see Bug 2673589 for an
      // example of this). This problem is noted in Sun Bug 6404011,
      //
      // http://bugs.sun.com/view_bug.do?bug_id=6404011
      //
      throw new BrokenImageException(name, e);
    }
    catch (ImageIOException e) {
      // Don't wrap ImageIOExceptions.
      throw e;
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
  }

  /** A functor for reading images. */
  protected static Wrapper<BufferedImage> readImage =
                                                 new Wrapper<BufferedImage>() {
    /**
     * Loads an image.
     *
     * @param name the image name
     * @param in the input stream
     * @return the image
     *
     * @throws UnrecognizedImageTypeException if the image type is unknown
     * @throws IOException if reading the image goes wrong
     */
    public BufferedImage run(String name, InputStream in) throws IOException {

      final BufferedImage img =
        ImageIO.read(new MemoryCacheImageInputStream(in));
      if (img == null) throw new UnrecognizedImageTypeException(name);

      return img;
    }
  };

  /** A functor for reading image dimensions. */
  protected static Wrapper<Dimension> readSize = new Wrapper<Dimension>() {
    /**
     * Gets the size of an image.
     *
     * @param name the image name
     * @param in the input stream
     * @return the size of the image
     *
     * @throws BrokenImageException if the image is faulty
     * @throws UnrecognizedImageTypeException if the image type is unknown
     * @throws IOException if reading the image goes wrong
     */
    public Dimension run(String name, InputStream in) throws IOException {
      final ImageInputStream stream = new MemoryCacheImageInputStream(in);

      final Iterator<ImageReader> i = ImageIO.getImageReaders(stream);
      if (!i.hasNext()) throw new UnrecognizedImageTypeException(name);

      final ImageReader reader = i.next();
      try {
        reader.setInput(stream);
        return new Dimension(reader.getWidth(0), reader.getHeight(0));
      }
      finally {
        reader.dispose();
      }
    }
  };

  protected BufferedImage fix_tRNS(Reference<BufferedImage> ref,
                                   int tRNS, int type) throws ImageIOException {
    BufferedImage img = ref.obj;

    // Ensure that we are working with integer ARGB data. Whether it's
    // premultiplied doesn't matter, since fully transparent black pixels
    // are the same in both.
    if (img.getType() != BufferedImage.TYPE_INT_ARGB &&
        img.getType() != BufferedImage.TYPE_INT_ARGB_PRE) {

      // If the requested type is not an ARGB one, then we convert to ARGB
      // for applying this fix.
      if (type != BufferedImage.TYPE_INT_ARGB &&
          type != BufferedImage.TYPE_INT_ARGB_PRE) {
        type = BufferedImage.TYPE_INT_ARGB;
      }

      img = null;
      img = tconv.convert(ref, type);
    }

    // NB: This unmanages the image.
    final DataBufferInt db = (DataBufferInt) img.getRaster().getDataBuffer();
    final int[] data = db.getData();

    // Set all pixels of the transparent color to have alpha 0.
    for (int i = 0; i < data.length; ++i) {
      if (data[i] == tRNS) data[i] = 0x00000000;
    }

    return img;
  }

  /**
   * Gets the size of an image.
   *
   * @param name the image name
   * @param in the input stream
   * @return the size of the image
   *
   * @throws BrokenImageException if the image is faulty
   * @throws UnrecognizedImageTypeException if the image type is not recognized
   * @throws ImageIOException if reading the image goes wrong
   */
  public Dimension size(String name, InputStream in) throws ImageIOException {
    return wrapImageIO(name, in, readSize);
  }
}
