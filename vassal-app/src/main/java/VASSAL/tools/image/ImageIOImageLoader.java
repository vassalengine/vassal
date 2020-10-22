/*
 *
 * Copyright (c) 2009-2013 by Joel Uckelman
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
import java.awt.color.ColorSpace;
import java.awt.color.ICC_Profile;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.MemoryCacheImageInputStream;

import VASSAL.tools.io.RereadableInputStream;
import VASSAL.tools.lang.Reference;

/**
 * An image loader which wraps {@link ImageIO}.
 *
 * This class handles the assorted problems with various versions of
 * {@link ImageIO}, ensuring that we can reliably load image files to
 * {link BufferedImages} with a predictable type.
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

  // Used to indicate whether this version of Java has the JPEG color
  // correction bug.
  protected static final boolean YCbCrBug;

  static {
    BufferedImage img;

    try (InputStream in = ImageIOImageLoader.class.getResourceAsStream("/images/black.jpg")) { //NON-NLS
      // We intentionally bypass the normal image loading system
      // in order to see how ImageIO loads the test image.
      img = ImageIO.read(new MemoryCacheImageInputStream(in));
    }
    catch (IOException e) {
      // this should not happen
      throw new IllegalStateException();
    }

    if (img == null) {
      // this should not happen
      throw new IllegalStateException();
    }

    // The pixel in the image is supposed to be black. If the pixel is
    // green, then ImageIO is misinterpreting the YCbCr data as RGB. If
    // the pixel is turquoise, then ImageIO is misinterpreting the color
    // in yet another way, which will also lead to same YCbCr mangling.
    // (So far the turquoise pixel happens only with 1.7.0_21 and 1.7.0_25
    // JVMs on Linux...)
    final int pixel = img.getRGB(0, 0);
    switch (pixel) {
    case 0xFF000000:
      YCbCrBug = false;
      break;
    case 0xFF008080:
    case 0xFF008700:
      YCbCrBug = true;
      break;
    default:
      // This JVM is broken in an unexpected way!
      throw new IllegalStateException(
        "Unexpected pixel value 0x" + String.format("%08x", pixel) //NON-NLS
      );
    }
  }

  // Workaround for Sun Bug 6986863:
  //
  //   http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6986863
  //
  // java.awt.color.ICC_Profile.getInstance() is static but isn't thread
  // safe (!) and is called from JPEGImageReader.getWidth(). This means that
  // not only is JPEGImageReader.getWidth() not thread-safe, but it's not
  // even thread-safe across different instances of JPEGImageReader. Nobody
  // will ever be trying to load more than one JPEG at a time, right? WTF?!
  //
  // ICC_Profile.getPCSType() calls ProfileDeferralMgr.activateProfiles(),
  // which sets ProfileDeferralMgr.deferring = false, which in turn prevents
  // further calls to ProfileDeferralMgr.activateProfiles(), which is where
  // the race happens.
  static {
    ICC_Profile.getInstance(ColorSpace.CS_sRGB).getPCSType();
  }

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
  @Override
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
    // Sun Bug 6444360: JPEGs with corrupt color profiles
    // Sun Bug 6404011: JPEGs with corrupt color profiles on Java 1.5
    // Sun Bug 4712797: YCbCr JPEGs with no JFIF marker
    // Sun Bug 4776576: YCbCr JPEFs with no JFIF marker
    //
    // http://bugs.sun.com/view_bug.do?bug_id=6788458
    // http://bugs.sun.com/view_bug.do?bug_id=6541476
    // http://bugs.sun.com/view_bug.do?bug_id=6444360
    // http://bugs.sun.com/view_bug.do?bug_id=6404011
    // http://bugs.sun.com/view_bug.do?bug_id=4712797
    // http://bugs.sun.com/view_bug.do?bug_id=4776576
    //
    // Someday, when both ImageIO is fixed and everyone's JRE contains
    // that fix, we can do this the simple way.
    //

    boolean fix_tRNS = false;
    int tRNS = 0x00000000;

    boolean fix_YCbCr = false;

    BufferedImage img = null;
    try (RereadableInputStream rin = new RereadableInputStream(in)) {
      rin.mark(512);

      DataInputStream din = new DataInputStream(rin);

      // Is this a PNG?
      if (PNGDecoder.decodeSignature(din)) {
        // The PNG chunks refered to here are defined in the PNG
        // standard, found at http://www.w3.org/TR/PNG/
        PNGDecoder.Chunk ch = PNGDecoder.decodeChunk(din);

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

            DONE_PNG: for (;;) {
              ch = PNGDecoder.decodeChunk(din);

              switch (ch.type) {
              case PNGDecoder.tRNS:
                fix_tRNS = true;
                break DONE_PNG;
              case PNGDecoder.IDAT:
                fix_tRNS = false;
                break DONE_PNG;
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
        }
      }
      else if (YCbCrBug) {
        rin.reset();
        rin.mark(512);

        din = new DataInputStream(rin);

        // Is this a JPEG?
        if (JPEGDecoder.decodeSignature(din)) {
          // The case where ImageIO fails is when there is no JFIF marker,
          // no color profile, and three color components with the same
          // subsampling. In this case, ImageIO incorrectly assumes that
          // this image is RGB instead of YCbCr.

          JPEGDecoder.Chunk ch;
          fix_YCbCr = true;

          DONE_JPEG: for (;;) {
            ch = JPEGDecoder.decodeChunk(din);

            switch (ch.type) {
            case JPEGDecoder.SOF0:
            case JPEGDecoder.SOF1:
            case JPEGDecoder.SOF2:
            case JPEGDecoder.SOF3:
            case JPEGDecoder.SOF4:
            case JPEGDecoder.SOF5:
            case JPEGDecoder.SOF6:
            case JPEGDecoder.SOF7:
            case JPEGDecoder.SOF9:
            case JPEGDecoder.SOF10:
            case JPEGDecoder.SOF11:
            case JPEGDecoder.SOF12:
            case JPEGDecoder.SOF13:
            case JPEGDecoder.SOF14:
            case JPEGDecoder.SOF15:
              // The JPEG standard requires any APPn markers to appear before
              // the first SOF marker, so if we see an SOF marker, we know
              // there are no APPn markers to find. Hence, we can decide now
              // whether this JPEG triggers the bug.
              fix_YCbCr =
                ch.data.length == 15 &&
                ch.data[5] == 3 &&    // color components
                ch.data[7] == ch.data[10] &&
                ch.data[7] == ch.data[13];
              break DONE_JPEG;

            case JPEGDecoder.APP0:
              if (ch.data.length >= 4 &&
                  ch.data[0] == 'J' &&
                  ch.data[1] == 'F' &&
                  ch.data[2] == 'I' &&
                  ch.data[3] == 'F') {
                // We've seen a JFIF, this image is ok.
                fix_YCbCr = false;
                break DONE_JPEG;
              }
              break;

            case JPEGDecoder.APP2:
              // Check whether we have a color profile. If so, then ImageIO
              // can handle decoding the image.
              if (ch.data.length >= 12 &&
                  ch.data[0]  == 'I' &&
                  ch.data[1]  == 'C' &&
                  ch.data[2]  == 'C' &&
                  ch.data[3]  == '_' &&
                  ch.data[4]  == 'P' &&
                  ch.data[5]  == 'R' &&
                  ch.data[6]  == 'O' &&
                  ch.data[7]  == 'F' &&
                  ch.data[8]  == 'I' &&
                  ch.data[9]  == 'L' &&
                  ch.data[10] == 'E' &&
                  ch.data[11] == 0x00) {
                // We have a color profile, this image is ok.
                fix_YCbCr = false;
                break DONE_JPEG;
              }
              break;

            case JPEGDecoder.APP13:
            case JPEGDecoder.APP14:
              // Created by Photoshop, this image is ok.
              fix_YCbCr = false;
              break DONE_JPEG;

            case JPEGDecoder.SOS:
              // We've reached a Start of Scan marker. Following this
              // is not a normal segment, but instead a lot of raw data.
              // This probably shouldn't happen with a valid JPEG.
            case JPEGDecoder.EOI:
              // We've reached the end. This probably shouldn't happen.
              break DONE_JPEG;

            default:
            }
          }
        }
      }

      // Load the image
      rin.reset();
      img = wrapImageIO(name, rin, readImage);
    }
    catch (ImageIOException e) {
      // Don't wrap ImageIOExceptions.
      throw e;
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }

    final int type =
      img.getTransparency() == BufferedImage.OPAQUE && !fix_tRNS
      ? typeIfOpaque : typeIfTransparent;

    final Reference<BufferedImage> ref = new Reference<>(img);

    if (fix_tRNS) {
      // Fix up transparency in type 2 Truecolor images.
      img = null;
      img = fix_tRNS(ref, tRNS, type);
      ref.obj = img;
    }
    else if (fix_YCbCr) {
      // Fix up color space in misinterpreted JPEGs.
      img = null;
      img = fix_YCbCr(ref, type);
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

  @FunctionalInterface
  protected interface Wrapper<T> {
    T run(String name, InputStream in) throws IOException;
  }

  protected <T> T wrapImageIO(String name, InputStream in, Wrapper<T> w)
                                                      throws ImageIOException {
    try {
      return w.run(name, in);
    }
    catch (ArrayIndexOutOfBoundsException | IllegalArgumentException | CMMException e) {
      // Note: ImageIO can throw an ArrayIndexOutOfBoundsException for
      // some corrupt JPEGs. This problem is noted in Sun Bug 6351707,
      //
      // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6351707
      //

      // Note: ImageIO can throw IllegalArgumentExceptions for certain
      // kinds of broken images, e.g., JPEGs which are in the RGB color
      // space but have non-RGB color profiles (see Bug 2673589 for an
      // example of this). This problem is noted in Sun Bug 6404011,
      // http://bugs.sun.com/view_bug.do?bug_id=6404011

      // Note: ImageIO can throw a CMMException for JPEGs which have
      // broken color profiles. This problem is noted in Sun Bugs 6444360
      // and 6839133.
      //
      // http://bugs.sun.com/view_bug.do?bug_id=6444360
      // http://bugs.sun.com/view_bug.do?bug_id=6839133
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
  protected static final Wrapper<BufferedImage> readImage = new Wrapper<>() {
    /**
     * Loads an image.
     *
     * @param name the image name
     * @param in the input stream
     * @return the image
     * @throws UnrecognizedImageTypeException if the image type is unknown
     * @throws IOException if reading the image goes wrong
     */
    @Override
    public BufferedImage run(String name, InputStream in) throws IOException {
      final BufferedImage img =
        ImageIO.read(new MemoryCacheImageInputStream(in));
      if (img == null) throw new UnrecognizedImageTypeException(name);

      return img;
    }
  };

  /** A functor for reading image dimensions. */
  protected static final Wrapper<Dimension> readSize = new Wrapper<>() {
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
    @Override
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

  protected BufferedImage fix_YCbCr(Reference<BufferedImage> ref, int type)
                                                      throws ImageIOException {
    BufferedImage img = ref.obj;

    // Ensure that we are working with RGB or ARGB data.
    if (img.getType() != BufferedImage.TYPE_INT_RGB &&
        img.getType() != BufferedImage.TYPE_INT_ARGB) {

      if (type != BufferedImage.TYPE_INT_RGB &&
          type != BufferedImage.TYPE_INT_ARGB) {
        type = BufferedImage.TYPE_INT_ARGB;
      }

      img = null;
      img = tconv.convert(ref, type);
    }

    // NB: This unmanages the image.
    final DataBufferInt db = (DataBufferInt) img.getRaster().getDataBuffer();
    final int[] data = db.getData();

    for (int i = 0; i < data.length; ++i) {
      final int y  =  (data[i] >> 16) & 0xFF;
      final int pb = ((data[i] >>  8) & 0xFF) - 128;
      final int pr = ( data[i]        & 0xFF) - 128;

      final int a  = (data[i] >> 24) & 0xFF;
      final int r = (int) Math.round(y + 1.402 * pr);
      final int g = (int) Math.round(y - 0.34414 * pb - 0.71414 * pr);
      final int b = (int) Math.round(y + 1.772 * pb);

      data[i] = (a << 24) |
                ((r < 0 ? 0 : (r > 0xFF ? 0xFF : r)) << 16) |
                ((g < 0 ? 0 : (g > 0xFF ? 0xFF : g)) <<  8) |
                 (b < 0 ? 0 : (b > 0xFF ? 0xFF : b));
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
  @Override
  public Dimension size(String name, InputStream in) throws ImageIOException {
    return wrapImageIO(name, in, readSize);
  }
}
