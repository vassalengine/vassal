/*
 * $Id$
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorConvertOp;
import java.awt.image.PixelGrabber;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.MemoryCacheImageInputStream;
import javax.swing.ImageIcon;

import org.jdesktop.swingx.graphics.GraphicsUtilities;

import VASSAL.build.BadDataReport;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.image.memmap.MappedBufferedImage;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.RereadableInputStream;

public class ImageUtils {
  private ImageUtils() {}

  // negative, because historically we've done it this way
  private static final double DEGTORAD = -Math.PI/180.0;

  public static final BufferedImage NULL_IMAGE = createCompatibleImage(1,1);

  private static final GeneralFilter.Filter upscale =
    new GeneralFilter.MitchellFilter();
  private static final GeneralFilter.Filter downscale =
    new GeneralFilter.Lanczos3Filter();

  public static final String PREFER_MEMORY_MAPPED = "preferMemoryMapped"; //$NON-NLS-1$
  private static final int MAPPED = 0;
  private static final int RAM = 1;
  private static int largeImageLoadMethod = RAM;

  public static boolean useMappedImages() {
    return largeImageLoadMethod == MAPPED;
  }

  public static final String SCALER_ALGORITHM = "scalerAlgorithm"; //$NON-NLS-1$ 
  private static final int MEDIUM = 1;
  private static final int GOOD = 2;
  private static int scalingQuality = GOOD;

  private static final Map<RenderingHints.Key,Object> defaultHints =
    new HashMap<RenderingHints.Key,Object>();

  static {
    // Initialise Image prefs prior to Preferences being read.
    setPreferMemoryMappedFiles(false);
    setHighQualityScaling(true);
    
    // set up map for creating default RenderingHints
    defaultHints.put(RenderingHints.KEY_INTERPOLATION,
                     RenderingHints.VALUE_INTERPOLATION_BILINEAR);
    defaultHints.put(RenderingClues.KEY_EXT_INTERPOLATION,
                     RenderingClues.VALUE_INTERPOLATION_LANCZOS_MITCHELL);
    defaultHints.put(RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON);
  } 

  public static void setPreferMemoryMappedFiles(boolean b) {
    largeImageLoadMethod = b ? MAPPED : RAM;
  }
  
  public static void setHighQualityScaling(boolean b) {
    final int newQual = b ? GOOD : MEDIUM;
    if (newQual != scalingQuality) {
      scalingQuality = newQual;
      Op.clearCache();

      defaultHints.put(RenderingClues.KEY_EXT_INTERPOLATION,
        scalingQuality == GOOD ?
          RenderingClues.VALUE_INTERPOLATION_LANCZOS_MITCHELL :
          RenderingClues.VALUE_INTERPOLATION_BILINEAR);
    }
  }
  
  public static RenderingHints getDefaultHints() {
    return new RenderingClues(defaultHints);
  }

  public static Rectangle transform(Rectangle srect,
                                    double scale,
                                    double angle) {
    final AffineTransform t = AffineTransform.getRotateInstance(
      DEGTORAD*angle, srect.getCenterX(), srect.getCenterY());
    t.scale(scale, scale);
    return t.createTransformedShape(srect).getBounds();
  }

  public static BufferedImage transform(BufferedImage src,
                                        double scale,
                                        double angle) {
    return transform(src, scale, angle,
                     getDefaultHints(),
                     scalingQuality);
  }

  public static BufferedImage transform(BufferedImage src,
                                        double scale,
                                        double angle,
                                        RenderingHints hints) {
    return transform(src, scale, angle, hints, scalingQuality);
  }

  public static BufferedImage transform(BufferedImage src,
                                        double scale,
                                        double angle,
                                        RenderingHints hints,
                                        int quality) {
    // bail on null source
    if (src == null) return null;

    // nothing to do, return source
    if (scale == 1.0 && angle == 0.0) {
      return src;
    }

    // return null image if scaling makes source vanish
    if (src.getWidth() * scale == 0 || src.getHeight() * scale == 0) {
      return NULL_IMAGE;
    }
  
    // use the default hints if we weren't given any
    if (hints == null) hints = getDefaultHints();

    if (scale == 1.0 && angle % 90.0 == 0.0) {
      // this is an unscaled quadrant rotation, we can do this simply
      hints.put(RenderingHints.KEY_INTERPOLATION,
                RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
      hints.put(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_OFF);
        
      final Rectangle ubox = getBounds(src);
      final Rectangle tbox = transform(ubox, scale, angle);

      // keep opaque destination for orthogonal rotation of an opaque source
      final BufferedImage trans = createCompatibleImage(
        tbox.width, tbox.height, src.getTransparency() != BufferedImage.OPAQUE
      );

      final AffineTransform t = new AffineTransform();
      t.translate(-tbox.x, -tbox.y);
      t.rotate(DEGTORAD*angle, ubox.getCenterX(), ubox.getCenterY());
      t.scale(scale, scale);
      t.translate(ubox.x, ubox.y);

      final Graphics2D g = trans.createGraphics();
      g.setRenderingHints(hints);
      g.drawImage(src, t, null);
      g.dispose();
      return trans;
    }
    else if (hints.get(RenderingClues.KEY_EXT_INTERPOLATION) ==
                       RenderingClues.VALUE_INTERPOLATION_LANCZOS_MITCHELL) {
      // do high-quality scaling
      if (angle != 0.0) {
        final Rectangle ubox = getBounds(src);
// FIXME: this duplicates the standard scaling case
// FIXME: check whether AffineTransformOp is faster

        final Rectangle rbox = transform(ubox, 1.0, angle);

        // keep opaque destination for orthogonal rotation of an opaque source
        final BufferedImage rot = new BufferedImage(
          rbox.width,
          rbox.height,
          src.getTransparency() == BufferedImage.OPAQUE && angle % 90.0 == 0.0 ?
            BufferedImage.TYPE_INT_RGB : BufferedImage.TYPE_INT_ARGB
        );

// FIXME: rotation via bilinear interpolation probably decreases quality
        final AffineTransform tx = new AffineTransform();
        tx.translate(-rbox.x, -rbox.y);
        tx.rotate(DEGTORAD*angle, ubox.getCenterX(), ubox.getCenterY());
        tx.translate(ubox.x, ubox.y);

        final Graphics2D g = rot.createGraphics();
        g.setRenderingHints(hints);
        g.drawImage(src, tx, null);
        g.dispose();
        src = rot;
      }
      else {
        src = coerceToIntType(src);
      }

      final Rectangle sbox = transform(getBounds(src), scale, 0.0);
      return GeneralFilter.zoom(sbox, src, scale > 1.0 ? upscale : downscale);
    }
    else {
      // do standard scaling
      final Rectangle ubox = getBounds(src);
      final Rectangle tbox = transform(ubox, scale, angle);

      // keep opaque destination for orthogonal rotation of an opaque source
      final BufferedImage trans = createCompatibleImage(
        tbox.width,
        tbox.height,
        src.getTransparency() == BufferedImage.OPAQUE && angle % 90.0 == 0.0
      );

      final AffineTransform t = new AffineTransform();
      t.translate(-tbox.x, -tbox.y);
      t.rotate(DEGTORAD*angle, ubox.getCenterX(), ubox.getCenterY());
      t.scale(scale, scale);
      t.translate(ubox.x, ubox.y);

      final Graphics2D g = trans.createGraphics();
      g.setRenderingHints(hints);
      g.drawImage(src, t, null);
      g.dispose();
      return trans;
    }
  }

  @SuppressWarnings("fallthrough")
  public static BufferedImage coerceToIntType(BufferedImage img) {
    // ensure that img is a type which GeneralFilter can handle
    switch (img.getType()) {
    case BufferedImage.TYPE_INT_RGB:  
    case BufferedImage.TYPE_INT_ARGB:
      return img;
    case BufferedImage.TYPE_CUSTOM:
      if (img instanceof MappedBufferedImage) return img;
    default:
      return toType(img, img.getTransparency() == BufferedImage.OPAQUE ?
        BufferedImage.TYPE_INT_RGB : BufferedImage.TYPE_INT_ARGB);
    }
  }

  /**
   * @param im 
   * @return the boundaries of this image, where (0,0) is the
   * pseudo-center of the image
   */
  public static Rectangle getBounds(BufferedImage im) {
    return new Rectangle(-im.getWidth()/2,
                         -im.getHeight()/2,
                          im.getWidth(),
                          im.getHeight());
  }

  public static Rectangle getBounds(Dimension d) {
    return new Rectangle(-d.width / 2,
                         -d.height / 2,
                          d.width,
                          d.height);
  }

  @Deprecated
  public static Dimension getImageSize(InputStream in) throws IOException {
    final ImageInputStream stream = new MemoryCacheImageInputStream(in);
    try {
      final Iterator<ImageReader> i = ImageIO.getImageReaders(stream);
      if (!i.hasNext()) throw new UnrecognizedImageTypeException();

      final ImageReader reader = i.next();
      try {
        reader.setInput(stream);
        final Dimension size =
          new Dimension(reader.getWidth(0), reader.getHeight(0));
        in.close();
        return size;
      }
      finally {
        reader.dispose();
      }
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  public static Dimension getImageSize(String name, InputStream in)
                                                      throws ImageIOException {
    try {
      final ImageInputStream stream = new MemoryCacheImageInputStream(in);

      final Iterator<ImageReader> i = ImageIO.getImageReaders(stream);
      if (!i.hasNext()) throw new UnrecognizedImageTypeException(name);

      final ImageReader reader = i.next();
      try {
        reader.setInput(stream);
        final Dimension size =
          new Dimension(reader.getWidth(0), reader.getHeight(0));
        in.close();
        return size;
      }
      finally {
        reader.dispose();
      }
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  /**
   * @deprecated Use {@link #getImage(String,InputStream)} instead.
   */ 
  @Deprecated
  public static BufferedImage getImage(InputStream in) throws IOException {
    final BufferedImage img = ImageIO.read(new MemoryCacheImageInputStream(in));
    if (img == null) throw new UnrecognizedImageTypeException();
    return toCompatibleImage(img);
  }

  public static boolean isMasked8BitRGBPNG(InputStream in) throws IOException {
    final DataInputStream din = new DataInputStream(in);

    // Bail immediately if this stream is not a PNG.
    if (!PNGDecoder.decodeSignature(din)) return false;

    // IHDR is required to be first, and tRNS is required to appear before
    // the first IDAT chunk; therefore, if we find an IDAT we're done.
    PNGDecoder.Chunk ch;

    ch = PNGDecoder.decodeChunk(din);
    if (ch.type != PNGDecoder.IHDR) return false;
    if (ch.data[8] != 8 || ch.data[9] != 2) return false;

    while (true) {
      ch = PNGDecoder.decodeChunk(din);

/*
      System.out.println(new char[]{
        (char)((ch.type >> 24) & 0xff),
        (char)((ch.type >> 16) & 0xff),
        (char)((ch.type >> 8) & 0xff),
        (char)(ch.type & 0xff)
      });
*/      

      // Numbers here refer to sections in the PNG standard, found
      // at http://www.w3.org/TR/PNG/
      switch (ch.type) {
      case PNGDecoder.tRNS:  // 11.3.2
        return true;      
      case PNGDecoder.IDAT:  // 12.2.4
        return false;
      default:
      }
    }
  }

  public static BufferedImage getImageResource(String name)
                                                      throws ImageIOException {
    final InputStream in = ImageUtils.class.getResourceAsStream(name);
    if (in == null) throw new ImageNotFoundException(name);
    return getImage(name, in);
  }

  public static BufferedImage getImage(String name, InputStream in)
                                                      throws ImageIOException {
    // FIXME: At present, ImageIO does not honor the tRNS chunk in 8-bit
    // color type 2 (RGB) PNGs. This is not a bug per se, as the PNG 
    // standard the does not require compliant decoders to use ancillary
    // chunks. However, every other PNG decoder we can find *does* honor
    // the tRNS chunk for this type of image, and so the appearance for
    // users is that VASSAL is broken when their 8-bit RGB PNGs don't show
    // the correct transparency.
    //
    // Therefore, we provide a workaround: Check the image metadata to see
    // whether we have an image which ImageIO will not handle fully, and if
    // we find one, load it using Toolkit.createImage() instead.
    // 
    // Someday, when both ImageIO is fixed and everyone's JRE contains
    // that fix, we can once again do this the simple way:
    //
    // final BufferedImage img =
    //   ImageIO.read(new MemoryCacheImageInputStream(in));
    // if (img == null) throw new UnrecognizedImageTypeException();
    // return toCompatibleImage(img);
    //
    // See Sun Bug 6788458, at
    // http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6788458
    //
    // Additionally, Toolkit.createImage() is unforgiving about malformed
    // PNGs, but instead of throwing an exception or returning null, it
    // returns a useless Image with negative width and height. (Though it
    // might also print a stack trace to the log.) There is at least one
    // piece of software (SplitImage) which writes tRNS chunks for type 2
    // images which are only 3 bytes long, and because this kind of thing
    // is used by module designers for slicing up scans of countersheets,
    // we can expect to see such crap from time to time. Therefore, if
    // Toolkit.createImage() fails, we fallback to ImageIO.read() and hope
    // for the best.
    //

    BufferedImage img = null;
    RereadableInputStream rin = null;
    try {
      rin = new RereadableInputStream(in);
      rin.mark(512);

      final boolean useToolkit = isMasked8BitRGBPNG(rin);
      rin.reset();

      if (useToolkit) {
        rin.mark(4096);

        final Image i =
          Toolkit.getDefaultToolkit().createImage(IOUtils.toByteArray(rin));

        // check that we received a valid Image from the Toolkit 
        if (i.getWidth(null) > 0 && i.getHeight(null) > 0) {
          img = toBufferedImage(i);
        }
        else {
          // Toolkit failed for some reason. Probably this means that
          // we have a broken PNG, so gently notify the user and fallback
          // to ImageIO.read().
          ErrorDialog.dataError(new BadDataReport("Broken PNG image", name));
          rin.reset();
        } 
      }

      if (img == null) {
        // Note: ImageIO can throw IllegalArgumentExceptions for certain
        // kinds of broken images, e.g., JPEGs which are in the RGB color
        // space but have non-RGB color profiles (see Bug 2673589 for an
        // example of this). We catch IllegalArgumentException here in
        // a last-ditch effort to prevent broken images---which are bad
        // data, not bugs---from causing an uncaught exception and raising
        // a Bug Dialog.
        try {
          img = ImageIO.read(new MemoryCacheImageInputStream(rin));
        }
        catch (IllegalArgumentException e) {
          ErrorDialog.dataError(new BadDataReport("Broken image", name));
          throw (IOException) new IOException().initCause(e);
        }

        if (img == null) throw new UnrecognizedImageTypeException();
        img = toCompatibleImage(img);
      }

      rin.close();

      return img;
    }
    catch (UnrecognizedImageTypeException e) {
      throw new UnrecognizedImageTypeException(name, e);
    }
    catch (IOException e) {
      throw new ImageIOException(name, e);
    }
    finally {
      IOUtils.closeQuietly(rin);
    }
  }

// FIXME: check speed
  private static BufferedImage colorConvertCopy(BufferedImage src,
                                                BufferedImage dst) {
    final ColorConvertOp op = new ColorConvertOp(
      src.getColorModel().getColorSpace(),
      dst.getColorModel().getColorSpace(), null);

    op.filter(src, dst);
    return dst;
  }

  public static BufferedImage toType(BufferedImage src, int type) {
    final BufferedImage dst =
      new BufferedImage(src.getWidth(), src.getHeight(), type);
    return colorConvertCopy(src, dst);
  }

  public static Image forceLoad(Image img) {
    // ensure that the image is loaded
    return new ImageIcon(img).getImage();
  }

  public static boolean isTransparent(Image img) {
    // determine whether this image has an alpha channel
    final PixelGrabber pg = new PixelGrabber(img, 0, 0, 1, 1, false);
    try {
      pg.grabPixels();
    }
    catch (InterruptedException e) {
      ErrorDialog.bug(e);
    }

    return pg.getColorModel().hasAlpha();
  }

  /**
   * Transform an <code>Image</code> to a <code>BufferedImage</code>.
   * 
   * @param src the <code>Image</code> to transform
   */
  public static BufferedImage toBufferedImage(Image src) {
    if (src == null) return null;
    if (src instanceof BufferedImage)
      return toCompatibleImage((BufferedImage) src);

    // ensure that the image is loaded
    src = forceLoad(src);

    final BufferedImage dst = createCompatibleImage(
      src.getWidth(null), src.getHeight(null), isTransparent(src)
    );

    final Graphics2D g = dst.createGraphics();
    g.drawImage(src, 0, 0, null);
    g.dispose();

    return dst;
  }

  public static BufferedImage createCompatibleImage(int w, int h) {
    return GraphicsUtilities.createCompatibleImage(w, h);  
  }

  public static BufferedImage createCompatibleImage(int w, int h,
                                                    boolean transparent) {
    return transparent ?
      GraphicsUtilities.createCompatibleTranslucentImage(w, h) :
      GraphicsUtilities.createCompatibleImage(w, h);
  }

  public static BufferedImage createCompatibleTranslucentImage(int w, int h) {
    return GraphicsUtilities.createCompatibleTranslucentImage(w, h);
  }

  public static BufferedImage toCompatibleImage(BufferedImage src) {
    return GraphicsUtilities.toCompatibleImage(src);
  }
  
  /*
   * What Image suffixes does Vassal know about?
   * Used by the MassPieceLoader to identify candidate images.
   */
  public static final String[] IMAGE_SUFFIXES = new String[] {
    ".gif", ".png", ".svg", ".jpg", ".jpeg"
  };                                                           
  
  public static boolean hasImageSuffix(String name) {
    final String s = name.toLowerCase();
    for (String suffix : IMAGE_SUFFIXES) {
      if (s.endsWith(suffix)) {
        return true;
      }
    }
    return false;
  }
  
  public static String stripImageSuffix(String name) {
    final String s = name.toLowerCase();
    for (String suffix : IMAGE_SUFFIXES) {
      if (s.endsWith(suffix)) {
        return name.substring(0, name.length()-suffix.length());
      }
    }
    return name;
  }
}
