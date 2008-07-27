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

package VASSAL.tools;

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorConvertOp;
import java.awt.image.ColorModel;
import java.awt.image.SampleModel;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.ImageReadParam;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.stream.FileCacheImageInputStream;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.MemoryCacheImageInputStream;

import VASSAL.build.GameModule;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.memmap.MappedBufferedImage;

public class ImageUtils {
  // negative, because historically we've done it this way
  private static final double DEGTORAD = -Math.PI/180.0;

  private static final BufferedImage NULL_IMAGE =
    new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);

  private static final GeneralFilter.Filter upscale =
    new GeneralFilter.MitchellFilter();
  private static final GeneralFilter.Filter downscale =
    new GeneralFilter.Lanczos3Filter();

  private static final ImageUtils instance = new ImageUtils();

  public static final String PREFER_MEMORY_MAPPED = "preferMemoryMapped"; //$NON-NLS-1$
  public static final String SCALER_ALGORITHM = "scalerAlgorithm"; //$NON-NLS-1$ 
  private static final int MAPPED = 0;
  private static final int RAM = 1;
  private int largeImageLoadMethod = RAM;

  private static final int MEDIUM = 1;
  private static final int GOOD = 2;
  private int scalingQuality = GOOD;

  private final Map<RenderingHints.Key,Object> defaultHints =
    new HashMap<RenderingHints.Key,Object>();

  private ImageUtils() {
    // create configurer for memory-mappedf file preference
    final BooleanConfigurer mappedPref = new BooleanConfigurer(
      PREFER_MEMORY_MAPPED,
      "Prefer memory-mapped files for large images?", //$NON-NLS-1$
      Boolean.FALSE);
//Resources.getString("GlobalOptions.smooth_scaling"), Boolean.TRUE); //$NON-NLS-1$
    mappedPref.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        largeImageLoadMethod =
          Boolean.TRUE.equals(mappedPref.getValue()) ? MAPPED : RAM;
      }
    });

    GameModule.getGameModule().getPrefs().addOption(mappedPref);

    // create configurer for scaling quality
    final BooleanConfigurer scalingPref = new BooleanConfigurer(
      SCALER_ALGORITHM,
      "High-quality scaling?", //$NON-NLS-1$ 
      Boolean.TRUE);
//      Resources.getString("GlobalOptions.smooth_scaling"), Boolean.TRUE); //$NON-NLS-1$
    scalingPref.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        final int newQual =
          Boolean.TRUE.equals(scalingPref.getValue()) ? GOOD : MEDIUM;
        if (newQual != scalingQuality) {
          scalingQuality = newQual;
          Op.clearCache();

          defaultHints.put(RenderingClues.KEY_EXT_INTERPOLATION,
            scalingQuality == GOOD ?
              RenderingClues.VALUE_INTERPOLATION_LANCZOS_MITCHELL :
              RenderingClues.VALUE_INTERPOLATION_BILINEAR);
        }
      }
    });

    GameModule.getGameModule().getPrefs().addOption(scalingPref);

    // set up map for creating default RenderingHints
    defaultHints.put(RenderingHints.KEY_INTERPOLATION,
                     RenderingHints.VALUE_INTERPOLATION_BILINEAR);
    defaultHints.put(RenderingClues.KEY_EXT_INTERPOLATION,
                     RenderingClues.VALUE_INTERPOLATION_LANCZOS_MITCHELL);
    defaultHints.put(RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON);
  } 

  public static RenderingHints getDefaultHints() {
    return new RenderingClues(instance.defaultHints);
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
                     instance.scalingQuality);
  }

  public static BufferedImage transform(BufferedImage src,
                                        double scale,
                                        double angle,
                                        RenderingHints hints) {
    return transform(src, scale, angle, hints, instance.scalingQuality);
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

      final BufferedImage trans =
        new BufferedImage(tbox.width, tbox.height,
                          BufferedImage.TYPE_INT_ARGB);

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

// FIXME: rotation via bilinear interpolation probably decreases quality
        final BufferedImage rot =
          new BufferedImage(rbox.width, rbox.height,
                            BufferedImage.TYPE_INT_ARGB);

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

/*
      // make sure that we have a TYPE_INT_ARGB before using GeneralFilter
      if (src.getType() != BufferedImage.TYPE_INT_ARGB) {
        src = toIntARGB(src);
      }
*/

      final Rectangle sbox = transform(getBounds(src), scale, 0.0);
      return GeneralFilter.zoom(sbox, src, scale > 1.0 ? upscale : downscale);
    }
    else {
      // do standard scaling
      final Rectangle ubox = getBounds(src);
      final Rectangle tbox = transform(ubox, scale, angle);

      final BufferedImage trans =
        new BufferedImage(tbox.width, tbox.height,
                          BufferedImage.TYPE_INT_ARGB);

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

  public static Dimension getImageSize(InputStream in) throws IOException {
    final ImageInputStream stream = new MemoryCacheImageInputStream(in);
    try {
      final Iterator<ImageReader> i = ImageIO.getImageReaders(stream);
      if (!i.hasNext()) throw new IOException("Unrecognized image format");

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

  public static Image getImage(InputStream in) throws IOException {
    return getSmallImage(in);
  }

  public static Image getSmallImage(InputStream in) throws IOException {
    final BufferedImage img = ImageIO.read(new MemoryCacheImageInputStream(in));
     
    return img.getType() != BufferedImage.TYPE_INT_ARGB
      ? ImageUtils.toIntARGBSmall(img) : img;
  }

  public static Image getLargeImage(InputStream in) throws IOException {
    final BufferedImage img = getLargeBufferedImage(in);

    return img.getType() != BufferedImage.TYPE_INT_ARGB ?
      ImageUtils.toIntARGBLarge(img) : img;
  }

  public static BufferedImage getLargeBufferedImage(InputStream in)
      throws IOException {
    BufferedImage img = null;

    // first try preferred storage type 
    int method = instance.largeImageLoadMethod;
    done:
    for (int tries = 0; tries < 2; ++tries, ++method) {
      final ImageInputStream stream = new FileCacheImageInputStream(
        in, TempFileManager.getInstance().getSessionRoot());
      try {
        final Iterator<ImageReader> i = ImageIO.getImageReaders(stream);
        if (!i.hasNext()) throw new IOException("Unrecognized image format");

        final ImageReader reader = i.next();
        try {
          reader.setInput(stream);
  
          final int w = reader.getWidth(0);
          final int h = reader.getHeight(0);
  
          switch (method % 2) {
          case MAPPED:
            try {
              final ImageTypeSpecifier type = reader.getImageTypes(0).next();
  
              // get our ColorModel and SampleModel
              final ColorModel cm = type.getColorModel();
              final SampleModel sm =
                type.getSampleModel().createCompatibleSampleModel(w,h);
      
              img = MappedBufferedImage.createMemoryMappedImage(cm, sm);
      
              final ImageReadParam param = reader.getDefaultReadParam();
              param.setDestination(img);
              reader.read(0, param);
              break done;
            }
            catch (IOException e) {
              // ignore, we throw an OutOfMemoryError at the bottom 
              ErrorLog.log(e);
            }
            break;
          case RAM:
            try {
              img = reader.read(0);
              break done;
            }
            catch (OutOfMemoryError e) {
              // ignore, we throw an OutOfMemoryError at the bottom 
              ErrorLog.log(e);
            }
            break;
          default:
            assert false;
          }  
        }
        finally {
          reader.dispose();
        }

        stream.close();
      }
      finally {
        IOUtils.closeQuietly(stream);
      }
    }

    if (img == null) throw new OutOfMemoryError();
  
    return img;
  }

  public static boolean isLargeImage(int w, int h) {
    return 4*w*h > 1024*1024;
  }

  private static BufferedImage rowByRowCopy(BufferedImage src,
                                            BufferedImage dst) {
    final int h = src.getHeight();
    final int[] row = new int[src.getWidth()];
    for (int y = 0; y < h; ++y) {
      src.getRGB(0, y, row.length, 1, row, 0, row.length);
      dst.setRGB(0, y, row.length, 1, row, 0, row.length);
    }
    return dst;
  }

  private static BufferedImage colorConvertCopy(BufferedImage src,
                                                BufferedImage dst) {
    final ColorConvertOp op = new ColorConvertOp(
      src.getColorModel().getColorSpace(),
      dst.getColorModel().getColorSpace(), null);

    op.filter(src, dst);
    return dst;
  }

  public static BufferedImage createEmptyLargeImage(int w, int h) {
    int method = instance.largeImageLoadMethod;
    for (int tries = 0; tries < 2; ++tries, ++method) {
      switch (method % 2) {
      case MAPPED:
        try {
          return MappedBufferedImage.createIntARGBMemoryMappedImage(w, h);
        }
        catch (IOException e) {
          // ignore, we throw an OutOfMemoryError at bottom
          ErrorLog.log(e);
        }
        break;
      case RAM:
        try {
          return new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
        }
        catch (OutOfMemoryError e) {
          // ignore, we throw an OutOfMemoryError at bottom
          ErrorLog.log(e);
        }
        break;
      default:
        assert false;
      }  
    }

    throw new OutOfMemoryError();
  }

  public static BufferedImage toIntARGBLarge(BufferedImage src) {
    final int w = src.getWidth();
    final int h = src.getHeight();

    int method = instance.largeImageLoadMethod;
    for (int tries = 0; tries < 2; ++tries, ++method) {
      switch (method % 2) {
      case MAPPED:
        try {
          return rowByRowCopy(src,
            MappedBufferedImage.createIntARGBMemoryMappedImage(w, h));
        }
        catch (IOException e) {
          // ignore, we throw an OutOfMemoryError at bottom
          ErrorLog.log(e);
        }
        break;
      case RAM:
        try {
          final BufferedImage dst =
            new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);

          return src instanceof MappedBufferedImage
            ? rowByRowCopy(src, dst) : colorConvertCopy(src, dst);
        }
        catch (OutOfMemoryError e) {
          // ignore, we throw an OutOfMemoryError at bottom
          ErrorLog.log(e);
        }
        break;
      default:
        assert false;
      }  
    }

    throw new OutOfMemoryError();
  }

  public static BufferedImage toIntARGBSmall(BufferedImage src) {
    final BufferedImage dst = new BufferedImage(
      src.getWidth(), src.getHeight(), BufferedImage.TYPE_INT_ARGB);
    return colorConvertCopy(src, dst);
  }

  public static BufferedImage toIntARGB(BufferedImage src) {
    return toIntARGBSmall(src);
  }

  /**
   * Create a memory-mapped image from an existing image.
   *
   * @param src the source image
   * @return a memory-mapped copy
   * @throws IOException if the memory-mapped image cannot be created
   */
  public static BufferedImage toMemoryMapped(Image src) throws IOException {
    final BufferedImage dst =
      MappedBufferedImage.createIntARGBMemoryMappedImage(
        src.getWidth(null), src.getHeight(null));

    final Graphics2D g = dst.createGraphics();
    g.drawImage(src, 0, 0, null);
    g.dispose();
    return dst;
  }

  /**
   * Transform an <code>Image</code> to a <code>BufferedImage</code>.
   * 
   * @param src the <code>Image</code> to transform
   */
  public static BufferedImage toBufferedImage(Image src) {
    if (src == null) return null;
    if (src instanceof BufferedImage) return (BufferedImage) src;

    final BufferedImage bi = new BufferedImage(src.getWidth(null),
                                               src.getHeight(null),
                                               BufferedImage.TYPE_INT_ARGB);
    final Graphics2D g = bi.createGraphics();
    g.drawImage(src, 0, 0, null);
    g.dispose();

    return bi;
  }
}
