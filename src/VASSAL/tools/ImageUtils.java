package VASSAL.tools;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorConvertOp;
import java.io.IOException;

public class ImageUtils {
  // negative, because historically we've done it this way
  private static final double DEGTORAD = -Math.PI/180.0;

  private static final BufferedImage NULL_IMAGE =
    new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);

  private static final GeneralFilter.Filter upscale =
    new GeneralFilter.MitchellFilter();
  private static final GeneralFilter.Filter downscale =
    new GeneralFilter.Lanczos3Filter();
  
  // prevent instantiation of this class
  private ImageUtils() { } 

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
    return transform(src, scale, angle, RenderingClues.getDefault());
  }

  public static BufferedImage transform(BufferedImage src,
                                        double scale,
                                        double angle,
                                        boolean quality) {
    RenderingClues rc = RenderingClues.getDefault();
    rc.put(RenderingClues.KEY_EXT_INTERPOLATION,
      quality ? RenderingClues.VALUE_INTERPOLATION_LANCZOS_MITCHELL :
                RenderingHints.VALUE_INTERPOLATION_BILINEAR);
    return transform(src, scale, angle, rc);
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
    if (hints == null) hints = RenderingClues.getDefault();
 
    if (src instanceof SVGManager.SVGBufferedImage) {
      // render SVG
      return ((SVGManager.SVGBufferedImage) src)
        .getTransformedInstance(scale, angle);
    }
    else if (scale == 0.0 && angle % 90.0 == 0.0) {
      // this is an unscaled quadrant rotation, we can do this simply
      hints.put(RenderingClues.KEY_INTERPOLATION,
                RenderingClues.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
      hints.put(RenderingClues.KEY_ANTIALIASING,
                RenderingClues.VALUE_ANTIALIAS_OFF);
        
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
    else if (RenderingClues.VALUE_INTERPOLATION_LANCZOS_MITCHELL.equals(
               hints.get(RenderingClues.KEY_EXT_INTERPOLATION))) {
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

      // make sure that we have a TYPE_INT_ARGB before using GeneralFilter
      if (src.getType() != BufferedImage.TYPE_INT_ARGB) {
        src = toIntARGB(src);
      }

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

  public static BufferedImage toIntARGB(BufferedImage src) {
    final BufferedImage dst =
      new BufferedImage(src.getWidth(), src.getHeight(),
                        BufferedImage.TYPE_INT_ARGB);
                              
    final ColorConvertOp op = new ColorConvertOp(
      src.getColorModel().getColorSpace(),
      dst.getColorModel().getColorSpace(), null);

    op.filter(src, dst);
    return dst;
  }
}
