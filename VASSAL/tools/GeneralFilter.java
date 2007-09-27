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

package VASSAL.tools;

import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.io.File;

import javax.imageio.ImageIO;

/*
   This class is the result of much trial and error with using timings
   and a profiler. Most things which are odd are that way because
   they're faster. Please profile any changes to ensure that they do not
   ruin our hard-won performance.
  
   Profiling information:
     java VASSAL.tools.GeneralFilter cc.png 0.406
  
   cc.png is a 3100x2500 32-bit image.
  
   ~32000         original version, not comitted
     1261   r32   precalculate xcontrib, ycontrib; large work[]
     1171   r33   precalculate ycontrib only, single-column work[]
      848   r34   check for constant-color areas
      727   r41   double -> float
      678   r45   copy scr part from src to Raster (not BufferedImage)
      659   r55   let zoom() create destination BufferedImage
*/  

/** 
 * GeneralFilter is a pluggable image resampler.
 * 
 * <p><code>GeneralFilter</code> up- or down-samples images or parts of
 * images using any one of the various filters contained in it as internal
 * classes.</p>
 *
 * <p><code>GeneralFilter</code> is based on <code>filter_rcg.c</code>, which
 * contains modifications made by Ray Gardener to <code>filter.c</code>,
 * originally by Dale Schumacher. <code>filter.c</code> appeared in 
 *
 * <blockquote>Dale Schumacher. "General Filtered Image Rescaling".
 * <em>Graphics Gems III</em>. David Kirk, ed. Academic Press. 1994.
 * pp. 8&ndash;16, 414&ndash;424.</blockquote>
 * 
 * and the source for <code>filter.c</code> and <code>filter_rcg.c</code>
 * are available
 * <a href="http://tog.acm.org/GraphicsGems/gems.html#gemsiii">here</a>.
 * Both <code>filter.c</code> and <code>filter_rcg.c</code> are in the
 * Public Domain.</p>
 *
 * <p>The filters provided here are intended for scaling, though other filters
 * could be created which resample in other ways.</p>
 *
 * @author Joel Uckelman
 */
public final class GeneralFilter {
  /** Do not instantiate this class. */
  private GeneralFilter() {}

  private static final class CList {
    public int n;            // number of source pixels
    public int[] pixel;      // source pixels
    public float[] weight;   // source pixel weights
  }

  /** The abstract base class for filters. */
  public static abstract class Filter {
    public abstract float getSamplingRadius();
    public abstract float apply(float t);
  }

  /** A Hermite filter. */ 
  public static final class HermiteFilter extends Filter {
    public float apply(float t) {
      // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
      if (t < 0.0f) t = -t;
      if (t < 1.0f) return (2.0f * t - 3.0f) * t * t + 1.0f;
      return 0.0f;
    }

    public float getSamplingRadius() { return 1.0f; }
  }

  /** A box filter. */ 
  public static final class BoxFilter extends Filter {
    public float apply(float t) {
      if (t > -0.5f && t <= 0.5f) return 1.0f;
      return 0.0f;
    }

    public float getSamplingRadius() { return 0.5f; }
  }

  /** A triangle, or bilinear, filter. */ 
  public static final class TriangleFilter {
    public float apply(float t) {
      if (t < 0.0f) t = -t;
      if (t < 1.0f) return 1.0f - t;
      return 0.0f;
    }

    public float getSamplingRadius() { return 1.0f; }
  }

  /** A Lanczos filter with radius 3. */ 
  public static final class Lanczos3Filter extends Filter {
    private float sinc(float t) {
      if (t != 0.0f) {
        t *= Math.PI;
        return (float)(Math.sin(t)/t); 
        // NB: casting sin(t) to float first is much slower.
      }
      else return 1.0f;
    }

    public float apply(float t) {
      if (t < 0.0f) t = -t;
      return t < 3.0f ? sinc(t) * sinc(t/3.0f) : 0.0f;
    }

    public float getSamplingRadius() { return 3.0f; }
  }

  /** A Mitchell filter. */
  public static final class MitchellFilter extends Filter {
    private static final float B = 1.0f/3.0f;
    private static final float C = 1.0f/3.0f;

    public float apply(float t) {
      float tt = t * t;
      
      if (t < 0.0f) t = -t;
      if (t < 1.0f) {
        t = (((12.0f - 9.0f * B - 6.0f * C) * (t * tt))
           + ((-18.0f + 12.0f * B + 6.0f * C) * tt)
           + (6.0f - 2.0f * B));
        return(t / 6.0f);
      }
      else if (t < 2.0f) {
        t = (((-1.0f * B - 6.0f * C) * (t * tt))
           + ((6.0f * B + 30.0f * C) * tt)
           + ((-12.0f * B - 48.0f * C) * t)
           + (8.0f * B + 24.0f * C));
        return(t / 6.0f);
      }
      return 0.0f;
    }

    public float getSamplingRadius() { return 2.0f; }
  }

  /** A Bell filter. */ 
  public static final class BellFilter extends Filter {
    public float apply(float t) {
      // box (*) box (*) box
      if (t < 0.0f) t = -t;
      if (t < 0.5f) return 0.75f - (t * t);
      if (t < 1.5f) {
        t -= 1.5f;
        return 0.5f * (t * t);
      }
      return 0.0f;
    }
    
    public float getSamplingRadius() { return 1.5f; }
  }

  /** A B-spline filter. */
  public static final class BSplineFilter extends Filter {
    public float apply(float t) {
      // box (*) box (*) box (*) box

      float tt;

      if(t < 0.0f) t = -t;

      if(t < 1.0f) {
        tt = t * t;
        return((0.5f * tt * t) - tt + (2.0f / 3.0f));
      }
      else if (t < 2.0f) {
        t = 2.0f - t;
        return((1.0f / 6.0f) * (t * t * t));
      }
      return 0.0f;
    }

    public float getSamplingRadius() { return 2.0f; }
  }

 /** Filters the entire source image.
   * <p>This is a convenience function which calls
   * {@link zoom(WritableRaster, Rectangle, BufferedImage, Filter)},
   * setting the destination rectangle as the bounds of the destination
   * tile.</p>
   * 
   * <p>Note that <code>src</code> <em>must</em> be be of type
   * <code>BufferedImage.TYPE_INT_ARGB</code> or everything will crash and
   * burn!</p>
   *
   * @param dst the destination rectangle
   * @param src the soure image
   * @param filter the filter to apply
   */ 
  public static BufferedImage zoom(Rectangle dst, BufferedImage src,
    final Filter filter) {

    final WritableRaster dstR =
      src.getColorModel()
         .createCompatibleWritableRaster(dst.width, dst.height);
    zoom(dstR, dstR.getBounds(), src, filter);
    // FIXME: check whether this affects hardware acceleration
    return new BufferedImage(src.getColorModel(), dstR, false, null);
  }

  /** Filters a portion of the source image.
   * Note that <code>dstR</code> <em>must</em> contain a
   * <code>DataBufferInt</code> and <code>srcI</code> <em>must</em>
   * be of type <code>BufferedImage.TYPE_INT_ARGB</code> or everything
   * will crash and burn!
   *
   * @param dstR the destination tile to calculate
   * @param dst the bounds of the whole destination image
   * @param srcI the source image
   * @param filter the filter to apply
   */
  public static void zoom(WritableRaster dstR,
                          Rectangle dst,
                          BufferedImage srcI,
                          final Filter filter) {

    final int dx0 = dstR.getMinX();
    final int dy0 = dstR.getMinY();
    final int dx1 = dx0 + dstR.getWidth() - 1;
    final int dy1 = dy0 + dstR.getHeight() - 1;
    final int dw = dx1 - dx0 + 1;
    final int dh = dy1 - dy0 + 1;

    final int dstWidth = dst.width;
    final int dstHeight = dst.height;

    final int srcWidth = srcI.getWidth();
    final int srcHeight = srcI.getHeight();

    // We want dstX0 * xscale = srcX0, except when that would make
    // xscale = 0; similarly for yscale.
    final float xscale =
      srcWidth == 1 ? dstWidth : (float)(dstWidth-1) / (srcWidth-1);
    final float yscale =
      srcHeight == 1 ? dstHeight : (float)(dstHeight-1) / (srcHeight-1);

    final float fwidth = filter.getSamplingRadius();

    final int sx0 = Math.max(0, (int) Math.floor((dx0-fwidth)/xscale));
    final int sy0 = Math.max(0, (int) Math.floor((dy0-fwidth)/yscale));
    final int sx1 = Math.min(srcWidth-1, (int) Math.ceil((dx1+fwidth)/xscale));
    final int sy1 = Math.min(srcHeight-1, (int) Math.ceil((dy1+fwidth)/yscale));
    final int sw = sx1 - sx0 + 1;
    final int sh = sy1 - sy0 + 1;

    // copy (sx0,sy0)-(sx1,sy1) to srcR
    final Raster srcR = srcI.getData(new Rectangle(sx0, sy0, sw, sh));
    final int src_data[] = ((DataBufferInt) srcR.getDataBuffer()).getData();

    final int work[] = new int[sh];
    final int dst_data[] = ((DataBufferInt) dstR.getDataBuffer()).getData();

    final CList[] ycontrib =
      calc_ycontrib(dh, fwidth, yscale, dy0, sy0, srcHeight, filter);
    final CList xcontrib = new CList();
    
    // apply the filter 
    for (int xx = 0; xx < dw; xx++) {
      calc_xcontrib(xscale, fwidth, xcontrib, xx, dx0, filter, srcWidth, sx0);
      apply_horizontal(sh, xcontrib, src_data, sw, work);
      apply_vertical(dh, ycontrib, work, dst_data, xx, dw);
    }
  }

  private static CList[] calc_ycontrib(final int dh,
                                       final float fwidth,
                                       final float yscale,
                                       final int dy0,
                                       final int sy0,
                                       final int srcHeight,
                                       final Filter filter) {
    // Calculate filter contributions for each destination column
    final CList[] ycontrib = new CList[dh];
    for (int i = 0; i < ycontrib.length; i++) { ycontrib[i] = new CList(); }

    if (yscale < 1.0f) {
      // Vertical sub-sampling, reduces image height
      final float width = fwidth / yscale;
      final float fscale = 1.0f / yscale;
      final int numContributors = (int)(width * 2.0f + 1);
      for (int i = 0; i < dh; i++) {
        ycontrib[i].n = 0;
        ycontrib[i].pixel = new int[numContributors];
        ycontrib[i].weight = new float[numContributors];

        final float center = (i+dy0) / yscale;
        final int left = (int) Math.floor(center - width);
        final int right = (int) Math.ceil(center + width);

        for (int j = left; j <= right; j++) {
          final float weight = filter.apply((center - j) / fscale) / fscale;
          if (weight == 0.0f) continue;

          final int n;
          if (j < 0) n = -j;
          else if (j >= srcHeight) n = srcHeight - j + srcHeight - 1;
          else n = j;

          final int k = ycontrib[i].n++;
          ycontrib[i].pixel[k] = n - sy0;
          ycontrib[i].weight[k] = weight;
        }
      }
    }
    else {
      // Vertical super-sampling, increases image height
      final int numContributors = (int)(fwidth * 2.0f + 1);
      for (int i = 0; i < dh; i++) {
        ycontrib[i].n = 0;
        ycontrib[i].pixel = new int[numContributors];
        ycontrib[i].weight = new float[numContributors];

        final float center = (i+dy0) / yscale;
        final int left = (int) Math.floor(center - fwidth);
        final int right = (int) Math.ceil(center + fwidth);

        for (int j = left; j <= right; j++) {
          final float weight = filter.apply(center - j);
          if (weight == 0.0f) continue;

          final int n;
          if (j < 0) n = -j;
          else if (j >= srcHeight) n = (srcHeight - j) + srcHeight - 1;
          else n = j;

          final int k = ycontrib[i].n++;
          ycontrib[i].pixel[k] = n - sy0;
          ycontrib[i].weight[k] = weight;
        }
      }
    }

    return ycontrib;
  }

  private static void calc_xcontrib(final float xscale,
                                    final float fwidth,
                                    final CList xcontrib,
                                    final int xx,
                                    final int dx0,
                                    final Filter filter,
                                    final int srcWidth, 
                                    final int sx0) {
    // Calculate filter contributions a destination row
    if (xscale < 1.0f) {
      // Horizontal sub-sampling, reduces image width
      final float width = fwidth / xscale;
      final float fscale = 1.0f / xscale;
      final int numPixels = (int)(width * 2.0f + 1);

      xcontrib.n = 0;
      xcontrib.pixel = new int[numPixels];
      xcontrib.weight = new float[numPixels];

      final float center = (xx+dx0) / xscale;
      final int left = (int) Math.floor(center - width);
      final int right = (int) Math.ceil(center + width);
      for (int j = left; j <= right; j++) {
        final float weight = filter.apply((center - j) / fscale) / fscale;
        if (weight == 0.0f) continue;

        final int n;
        if (j < 0) n = -j;
        else if (j >= srcWidth) n = (srcWidth - j) + srcWidth - 1;
        else n = j;

        final int k = xcontrib.n++;
        xcontrib.pixel[k] = n - sx0;
        xcontrib.weight[k] = weight;
      }
    }
    else {
      // Horizontal super-sampling, increases image width
      final int numPixels = (int)(fwidth * 2.0f + 1);
      xcontrib.n = 0;
      xcontrib.pixel = new int[numPixels];
      xcontrib.weight = new float[numPixels];

      final float center = (xx+dx0) / xscale;
      final int left = (int) Math.floor(center - fwidth);
      final int right = (int) Math.ceil(center + fwidth);
      for (int j = left; j <= right; j++) {
        final float weight = filter.apply(center - j);
        if (weight == 0.0f) continue;

        final int n;
        if (j < 0) n = -j;
        else if (j >= srcWidth) n = srcWidth - j + srcWidth - 1;
        else n = j;
          
        final int k = xcontrib.n++;
        xcontrib.pixel[k] = n - sx0;
        xcontrib.weight[k] = weight;
      }
    }
  }

  private static void apply_horizontal(final int sh,
                                       final CList xcontrib,
                                       final int[] src_data,
                                       final int sw,
                                       final int[] work) {
    // Apply pre-computed filter to sample horizontally from src to work
    for (int k = 0; k < sh; k++) {
      float s_a = 0.0f;  // alpha sample
      float s_r = 0.0f;  // red sample
      float s_g = 0.0f;  // green sample
      float s_b = 0.0f;  // blue sample
        
      final CList c = xcontrib;
      final int max = c.n;
      final int pel = src_data[c.pixel[0] + k*sw];
      boolean bPelDelta = false;

      // Check for areas of constant color. It is *much* faster to
      // to check first and then calculate weights only if needed.
      for (int j = 0; j < max; j++) {
        final float w = c.weight[j];
        if (w == 0.0f) continue;

        final int sd = src_data[c.pixel[j] + k*sw];
        if (sd != pel) { bPelDelta = true; break; }
      }

      if (bPelDelta) {
        // There is a color change from 0 to max; we need to compute weights.
        for (int j = 0; j < max; j++) {
          final float w = c.weight[j];
          if (w == 0.0f) continue;

          final int sd = src_data[c.pixel[j] + k*sw];

          s_a += ((sd >> 24) & 0xff) * w;
          s_r += ((sd >> 16) & 0xff) * w;
          s_g += ((sd >>  8) & 0xff) * w;
          s_b += ( sd        & 0xff) * w;
        }

        // Ugly, but fast.        
        work[k] =
         (s_a < 0 ? 0 : s_a > 255 ? 255 : (int)(s_a+0.5f)) << 24 |
         (s_r < 0 ? 0 : s_r > 255 ? 255 : (int)(s_r+0.5f)) << 16 |
         (s_g < 0 ? 0 : s_g > 255 ? 255 : (int)(s_g+0.5f)) <<  8 |
         (s_b < 0 ? 0 : s_b > 255 ? 255 : (int)(s_b+0.5f));
      }
      else {
        // If there's no color change from 0 to max, maintain that.
        work[k] = pel;
      }
    } 
  }

  private static void apply_vertical(final int dh,
                                     final CList[] ycontrib,
                                     final int[] work,
                                     final int[] dst_data,
                                     final int xx,
                                     final int dw) {
    // Apply pre-computed filter to sample vertically from work to dst
    for (int i = 0; i < dh; i++) {
      float s_a = 0.0f;  // alpha sample
      float s_r = 0.0f;  // red sample
      float s_g = 0.0f;  // green sample
      float s_b = 0.0f;  // blue sample

      final CList c = ycontrib[i];
      final int max = c.n;
      final int pel = work[c.pixel[0]];
      boolean bPelDelta = false;

      // Check for areas of constant color. It is *much* faster to
      // to check first and then calculate weights only if needed.
      for (int j = 0; j < max; j++) {
        final float w = c.weight[j];
        if (w == 0.0f) continue;

        final int wd = work[c.pixel[j]];
        if (wd != pel) { bPelDelta = true; break; }
      }

      if (bPelDelta) {
        // There is a color change from 0 to max; we need to compute weights.
        for (int j = 0; j < max; j++) {
          final float w = c.weight[j];
          if (w == 0.0f) continue;

          final int wd = work[c.pixel[j]];
          if (wd != pel) bPelDelta = true;

          s_a += ((wd >> 24) & 0xff) * w;
          s_r += ((wd >> 16) & 0xff) * w;
          s_g += ((wd >>  8) & 0xff) * w;
          s_b += ( wd        & 0xff) * w;
        }

        // Ugly, but fast.
        dst_data[xx + i*dw] = 
         (s_a < 0 ? 0 : s_a > 255 ? 255 : (int)(s_a+0.5f)) << 24 |
         (s_r < 0 ? 0 : s_r > 255 ? 255 : (int)(s_r+0.5f)) << 16 |
         (s_g < 0 ? 0 : s_g > 255 ? 255 : (int)(s_g+0.5f)) <<  8 |
         (s_b < 0 ? 0 : s_b > 255 ? 255 : (int)(s_b+0.5f));
      }
      else {
        // If there's no color change from 0 to max, maintain that.
        dst_data[xx + i*dw] = pel; 
      }
    }
  }

  /** A program for running filter benchmarks. */
  public static void main(String[] args) throws java.io.IOException {
    BufferedImage src = null;
    try {
      src = ImageIO.read(new File(args[0]));
    }
    catch (Exception e) {
    }

    float scale = Float.parseFloat(args[1]);

    int newwidth = (int) (src.getWidth() * scale);
    int newheight = (int) (src.getHeight() * scale);

    if (src.getType() != BufferedImage.TYPE_INT_ARGB) {
      String type = null;
      switch (src.getType()) {
      case BufferedImage.TYPE_3BYTE_BGR:
        type = "TYPE_3BYTE_BGR"; break;
      case BufferedImage.TYPE_4BYTE_ABGR:
        type = "TYPE_4BYTE_ABGR"; break;
      case BufferedImage.TYPE_4BYTE_ABGR_PRE:
        type = "TYPE_4BYTE_ABGR_PRE"; break;
      case BufferedImage.TYPE_BYTE_BINARY:
        type = "TYPE_BYTE_BINARY"; break;
      case BufferedImage.TYPE_BYTE_GRAY:
        type = "TYPE_BYTE_GRAY"; break;
      case BufferedImage.TYPE_BYTE_INDEXED:
        type = "TYPE_BYTE_INDEXED"; break;
      case BufferedImage.TYPE_CUSTOM:
        type = "TYPE_CUSTOM"; break;
      case BufferedImage.TYPE_INT_ARGB_PRE:
        type = "TYPE_INT_ARGB_PRE"; break;
      case BufferedImage.TYPE_INT_BGR:
        type = "TYPE_INT_BGR"; break;
      case BufferedImage.TYPE_INT_RGB:
        type = "TYPE_INT_RGB"; break;
      case BufferedImage.TYPE_USHORT_555_RGB:
        type = "TYPE_USHORT_555_RGB"; break;
      case BufferedImage.TYPE_USHORT_565_RGB:
        type = "TYPE_USHORT_565_RGB"; break;
      case BufferedImage.TYPE_USHORT_GRAY:
        type = "TYPE_USHORT_GRAY"; break;
      }
      System.out.println("src is a " + type + ", converting...");

      // convert to TYPE_INT_ARGB if not already
      BufferedImage tmp = new BufferedImage(src.getWidth(),
                                            src.getHeight(),
                                            BufferedImage.TYPE_INT_ARGB);
      Graphics g = tmp.getGraphics();
      g.drawImage(src, 0, 0, null);
      g.dispose();
 
      src = tmp;
    }

//    WritableRaster dst = null;
    @SuppressWarnings("unused")
    BufferedImage dst = null;

    @SuppressWarnings("unused")
    final int dx0 = 0;
    @SuppressWarnings("unused")
    final int dy0 = 0;
    final int dw = newwidth; 
    final int dh = newheight; 

    for (int i = 0; i < 40; ++i) {
      long start = System.currentTimeMillis();
/*
      dst =
        (new BufferedImage(newwidth, newheight, BufferedImage.TYPE_INT_ARGB))
          .getData().createCompatibleWritableRaster(dx0, dy0, dw, dh);
      GeneralFilter.zoom(dst, dst.getBounds(), src, new Lanczos3Filter());
*/
      dst = zoom(new Rectangle(0, 0, dw, dh), src, new Lanczos3Filter());
      System.out.println(System.currentTimeMillis() - start);
    }

    System.out.println("Ready...");
    System.in.read();
    System.out.println("Starting...");

    long time = 0;
    for (int i = 0; i < 10; ++i) {
      long start = System.currentTimeMillis();
/*
      dst =
        (new BufferedImage(newwidth, newheight, BufferedImage.TYPE_INT_ARGB))
          .getData().createCompatibleWritableRaster(dx0, dy0, dw, dh);
      GeneralFilter.zoom(dst, dst.getBounds(), src, new Lanczos3Filter());
*/
      dst = zoom(new Rectangle(0, 0, dw, dh), src, new Lanczos3Filter());
      time += System.currentTimeMillis() - start;
    }
    
    System.out.println(time/10);

    System.out.println("Done.");
    System.in.read();

/*
    try {
      ImageIO.write(dst, "png", new File("dst.png"));
    }
    catch (Exception e) {
    }
*/
  }
}
