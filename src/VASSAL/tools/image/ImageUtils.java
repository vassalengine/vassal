/*
 * $Id$
 *
 * Copyright (c) 2007-2010 by Joel Uckelman
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
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.PixelGrabber;
import java.awt.image.WritableRaster;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.swing.ImageIcon;

import VASSAL.Info;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.io.TemporaryFileFactory;

public class ImageUtils {
  private ImageUtils() {}

  // FIXME: We should fix this, eventually.
  // negative, because historically we've done it this way
  private static final double DEGTORAD = -Math.PI/180.0;

  private static final GeneralFilter.Filter upscale =
    new GeneralFilter.MitchellFilter();
  private static final GeneralFilter.Filter downscale =
    new GeneralFilter.Lanczos3Filter();

  @Deprecated
  public static final String SCALER_ALGORITHM = "scalerAlgorithm"; //$NON-NLS-1$
  private static final Map<RenderingHints.Key,Object> defaultHints =
    new HashMap<RenderingHints.Key,Object>();

  static {
    // Initialise Image prefs prior to Preferences being read.

    // set up map for creating default RenderingHints
    defaultHints.put(RenderingHints.KEY_INTERPOLATION,
                     RenderingHints.VALUE_INTERPOLATION_BILINEAR);
    defaultHints.put(RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON);
  }

  /** @deprecated All scaling is done with the high-quality scaler now. */
  @Deprecated
  public static void setHighQualityScaling(boolean b) {}

  public static RenderingHints getDefaultHints() {
    return new RenderingHints(defaultHints);
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
    return transform(src, scale, angle, getDefaultHints());
  }

  public static BufferedImage transform(BufferedImage src,
                                        double scale,
                                        double angle,
                                        RenderingHints hints) {
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
        tbox.width,
        tbox.height,
        src.getTransparency() != BufferedImage.OPAQUE
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
    else {
      if (angle != 0.0) {
        final Rectangle ubox = getBounds(src);
// FIXME: this duplicates the standard scaling case
// FIXME: check whether AffineTransformOp is faster

        final Rectangle rbox = transform(ubox, 1.0, angle);

        // keep opaque destination for orthogonal rotation of an opaque source
        final BufferedImage rot = createCompatibleImage(
          rbox.width,
          rbox.height,
          src.getTransparency() != BufferedImage.OPAQUE || angle % 90.0 != 0.0
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

      if (scale != 1.0) {
        src = coerceToIntType(src);

        final Rectangle sbox = transform(getBounds(src), scale, 0.0);

        // return null image if scaling makes source vanish
        if (sbox.width == 0 || sbox.height == 0) {
          return NULL_IMAGE;
        }

        final BufferedImage dst =
          GeneralFilter.zoom(sbox, src, scale > 1.0 ? upscale : downscale);

        return toCompatibleImage(dst);
      }
      else {
        return src;
      }
    }
  }

  @Deprecated
  public static BufferedImage transform(BufferedImage src,
                                        double scale,
                                        double angle,
                                        RenderingHints hints,
                                        int quality) {
    return transform(src, scale, angle, hints);
  }

  @SuppressWarnings("fallthrough")
  public static BufferedImage coerceToIntType(BufferedImage img) {
    // ensure that img is a type which GeneralFilter can handle
    switch (img.getType()) {
    case BufferedImage.TYPE_INT_RGB:
    case BufferedImage.TYPE_INT_ARGB:
    case BufferedImage.TYPE_INT_ARGB_PRE:
    case BufferedImage.TYPE_INT_BGR:
      return img;
    default:
      return toType(img, img.getTransparency() == BufferedImage.OPAQUE ?
        BufferedImage.TYPE_INT_RGB :
        getCompatibleTranslucentImageType() == BufferedImage.TYPE_INT_ARGB ?
          BufferedImage.TYPE_INT_ARGB : BufferedImage.TYPE_INT_ARGB_PRE);
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

  /** @deprecated Use {@link #getImageSize(String,InputStream)} instead. */
  @Deprecated
  public static Dimension getImageSize(InputStream in) throws IOException {
    return getImageSize("", in);
  }

  private static final TemporaryFileFactory tfac = new TemporaryFileFactory() {
    public File create() throws IOException {
      return File.createTempFile("img", null, Info.getTempDir());
    }
  };

  private static final ImageLoader loader =
    new ImageIOImageLoader(new FallbackImageTypeConverter(tfac));

  public static Dimension getImageSize(String name, InputStream in)
                                                      throws ImageIOException {
    return loader.size(name, in);
  }

  /** @deprecated Use {@link #getImage(String,InputStream)} instead. */
  @Deprecated
  public static BufferedImage getImage(InputStream in) throws IOException {
    return getImage("", in);
  }

  public static BufferedImage getImageResource(String name)
                                                      throws ImageIOException {
    final InputStream in = ImageUtils.class.getResourceAsStream(name);
    if (in == null) throw new ImageNotFoundException(name);
    return getImage(name, in);
  }

  public static BufferedImage getImage(String name, InputStream in)
                                                      throws ImageIOException {
    return loader.load(
      name, in, compatOpaqueImageType, compatTranslImageType, true
    );
  }

  public static BufferedImage toType(BufferedImage src, int type) {
    final BufferedImage dst =
      new BufferedImage(src.getWidth(), src.getHeight(), type);

    final Graphics2D g = dst.createGraphics();
    g.drawImage(src, 0, 0, null);
    g.dispose();

    return dst;
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

  public static boolean isTransparent(BufferedImage img) {
    return img.getTransparency() != BufferedImage.OPAQUE;
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

  protected static final boolean IS_MAC_RETINA;

  static {
    final Object o = Toolkit.getDefaultToolkit().getDesktopProperty(
      "apple.awt.contentScaleFactor"
    );

    IS_MAC_RETINA = (o instanceof Number) && ((Number) o).doubleValue() == 2.0;
  }

  public static boolean isMacRetina() {
    return IS_MAC_RETINA;
  }

  private static boolean isHeadless() {
    return GraphicsEnvironment.isHeadless();
  }

  private static GraphicsConfiguration getGraphicsConfiguration() {
    return GraphicsEnvironment.getLocalGraphicsEnvironment().
             getDefaultScreenDevice().getDefaultConfiguration();
  }

  protected static final BufferedImage compatOpaqueImage;
  protected static final BufferedImage compatTransImage;

  protected static final int compatOpaqueImageType;
  protected static final int compatTranslImageType;

  static {
    BufferedImage oimg;
    BufferedImage timg;

    if (isHeadless()) {
      oimg = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB);
      timg = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);
    }
    else {
      final GraphicsConfiguration gc = getGraphicsConfiguration();

      oimg = gc.createCompatibleImage(1,1, BufferedImage.OPAQUE);
      timg = gc.createCompatibleImage(1,1, BufferedImage.TRANSLUCENT);

      // Bug workaround: MacOX X machines with Retina displays are incapable
      // of painting TYPE_INT_ARGB_PRE images, despite that these systems
      // return that type as the "compatible" image type.
      if (isMacRetina()) {
        if (oimg.getType() == BufferedImage.TYPE_INT_ARGB_PRE) {
          oimg = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);
        }

        if (timg.getType() == BufferedImage.TYPE_INT_ARGB_PRE) {
          timg = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);
        }
      }
    }

    compatOpaqueImage = oimg;
    compatTransImage = timg;

    compatOpaqueImageType = compatOpaqueImage.getType();
    compatTranslImageType = compatTransImage.getType();
  }

  public static final BufferedImage NULL_IMAGE = createCompatibleImage(1,1);

  public static int getCompatibleImageType() {
    return compatOpaqueImageType;
  }

  public static int getCompatibleTranslucentImageType() {
    return compatTranslImageType;
  }

  public static int getCompatibleImageType(boolean transparent) {
    return transparent ? compatTranslImageType : compatOpaqueImageType;
  }

  public static int getCompatibleImageType(BufferedImage img) {
    return getCompatibleImageType(isTransparent(img));
  }

  public static BufferedImage createCompatibleImage(int w, int h) {
    final ColorModel cm = compatOpaqueImage.getColorModel();
    final WritableRaster wr = cm.createCompatibleWritableRaster(w, h);
    return new BufferedImage(cm, wr, cm.isAlphaPremultiplied(), null);
  }

  public static BufferedImage createCompatibleImage(int w, int h,
                                                    boolean transparent) {
    return transparent ?
      createCompatibleTranslucentImage(w, h) :
      createCompatibleImage(w, h);
  }

  public static BufferedImage createCompatibleTranslucentImage(int w, int h) {
    final ColorModel cm = compatTransImage.getColorModel();
    final WritableRaster wr = cm.createCompatibleWritableRaster(w, h);
    return new BufferedImage(cm, wr, cm.isAlphaPremultiplied(), null);
  }

  public static BufferedImage toCompatibleImage(BufferedImage src) {
    if ((src.getColorModel().equals(compatOpaqueImage.getColorModel()) &&
         src.getTransparency() == compatOpaqueImage.getTransparency())
        ||
        (src.getColorModel().equals(compatTransImage.getColorModel()) &&
         src.getTransparency() == compatTransImage.getTransparency()))
    {
      return src;
    }

    final BufferedImage dst = createCompatibleImage(
      src.getWidth(), src.getHeight(), isTransparent(src)
    );

    final Graphics2D g = dst.createGraphics();
    g.drawImage(src, 0, 0, null);
    g.dispose();

    return dst;
  }

  public static boolean isCompatibleImage(BufferedImage img) {
    return img.getType() ==
      getCompatibleImageType(img.getTransparency() != BufferedImage.OPAQUE);
  }

  /*
   * What Image suffixes does Vassal know about?
   * Used by the MassPieceLoader to identify candidate images.
   */
  public static final String GIF_SUFFIX = ".gif";
  public static final String PNG_SUFFIX = ".png";
  public static final String SVG_SUFFIX = ".svg";
  public static final String JPG_SUFFIX = ".jpg";
  public static final String JPEG_SUFFIX = ".jpeg";
  public static final String[] IMAGE_SUFFIXES = {
    GIF_SUFFIX, PNG_SUFFIX, SVG_SUFFIX, JPG_SUFFIX, JPEG_SUFFIX
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
