/*
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

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.awt.image.WritableRaster;

/*
   This class is the result of much trial and error with using timings
   and a profiler. Most things which are odd are that way because
   they're faster. Please profile any changes to ensure that they do not
   ruin our hard-won performance.

   Profiling information:

java -Xmx1024M -cp classes VASSAL.tools.image.GeneralFilterTest cc.png 0.406 0
java -Xmx1024M -cp classes VASSAL.tools.image.GeneralFilterTest cc.png 0.406 1
java -Xmx1024M -cp classes VASSAL.tools.image.GeneralFilterTest cc.png 0.406 2

   cc.png is a 3100x2500 32-bit image.

   RGBA PRE RGB
  32000               original version, not comitted
   1261         r32   precalculate xcontrib, ycontrib; large work[]
   1171         r33   precalculate ycontrib only, single-column work[]
    848         r34   check for constant-color areas
    727         r41   double -> float
    678         r45   copy scr part from src to Raster (not BufferedImage)
    659         r55   let zoom() create destination BufferedImage
    777         r65   downsampling narrow, short images fixed; upsampling fixed
    699         r2494 split cases for with/without alpha channel
    518         r5516 removed useless checks, removed useless CList field
    529 532 466 r5894 added case for premultiplied ARGB
    525 597 468 r5896 correctly premultiply, scale, unpremultiply for ARGB
    535 483 408 r7416 don't copy src rectangle, use src data directly
    531 476 412 r7417 moved common expressions out of loops
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
    public int pixel;        // starting source pixel
    public float[] weight;   // source pixel weights
  }

  /** The abstract base class for filters. */
  public static abstract class Filter {
    public abstract float getSamplingRadius();
    public abstract float apply(float t);
  }

  /** A Hermite filter. */
  public static final class HermiteFilter extends Filter {
    @Override
    public float apply(float t) {
      // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
      if (t < 0.0f) t = -t;
      if (t < 1.0f) return (2.0f * t - 3.0f) * t * t + 1.0f;
      return 0.0f;
    }

    @Override
    public float getSamplingRadius() {
      return 1.0f;
    }
  }

  /** A box filter. */
  public static final class BoxFilter extends Filter {
    @Override
    public float apply(float t) {
      if (t > -0.5f && t <= 0.5f) return 1.0f;
      return 0.0f;
    }

    @Override
    public float getSamplingRadius() {
      return 0.5f;
    }
  }

  /** A triangle, or bilinear, filter. */
  public static final class TriangleFilter {
    public float apply(float t) {
      if (t < 0.0f) t = -t;
      if (t < 1.0f) return 1.0f - t;
      return 0.0f;
    }

    public float getSamplingRadius() {
      return 1.0f;
    }
  }

  /** A Lanczos filter with radius 3. */
  public static final class Lanczos3Filter extends Filter {
    private float sinc(float t) {
      if (t == 0.0f) return 1.0f;
      t *= Math.PI;
      return (float)(Math.sin(t)/t);
    }

    @Override
    public float apply(float t) {
      if (t < -3.0f) return 0.0f;
      if (t <  0.0f) return sinc(-t) * sinc(-t/3.0f);
      if (t <  3.0f) return sinc( t) * sinc( t/3.0f);
      return 0.0f;
    }

    @Override
    public float getSamplingRadius() {
      return 3.0f;
    }
  }

  /** A Mitchell filter. */
  public static final class MitchellFilter extends Filter {
    private static final float B = 1.0f/3.0f;
    private static final float C = 1.0f/3.0f;
    private static final float P0 = (  6.0f- 2.0f*B        )/6.0f;
    private static final float P2 = (-18.0f+12.0f*B+ 6.0f*C)/6.0f;
    private static final float P3 = ( 12.0f- 9.0f*B- 6.0f*C)/6.0f;
    private static final float Q0 = (        8.0f*B+24.0f*C)/6.0f;
    private static final float Q1 = (      -12.0f*B-48.0f*C)/6.0f;
    private static final float Q2 = (        6.0f*B+30.0f*C)/6.0f;
    private static final float Q3 = (      - 1.0f*B- 6.0f*C)/6.0f;

    @Override
    public float apply(float t) {
      if (t < -2.0f) return 0.0f;
      if (t < -1.0f) return Q0-t*(Q1-t*(Q2-t*Q3));
      if (t <  0.0f) return P0+t*t*(P2-t*P3);
      if (t <  1.0f) return P0+t*t*(P2+t*P3);
      if (t <  2.0f) return Q0+t*(Q1+t*(Q2+t*Q3));
      return 0.0f;
    }

    @Override
    public float getSamplingRadius() {
      return 2.0f;
    }
  }

  /** A Bell filter. */
  public static final class BellFilter extends Filter {
    @Override
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

    @Override
    public float getSamplingRadius() {
      return 1.5f;
    }
  }

  /** A B-spline filter. */
  public static final class BSplineFilter extends Filter {
    @Override
    public float apply(float t) {
      // box (*) box (*) box (*) box

      if(t < 0.0f) t = -t;

      if(t < 1.0f) {
        final float tt = t * t;
        return (0.5f * tt * t) - tt + (2.0f / 3.0f);
      }
      else if (t < 2.0f) {
        t = 2.0f - t;
        return (1.0f / 6.0f) * (t * t * t);
      }

      return 0.0f;
    }

    @Override
    public float getSamplingRadius() {
      return 2.0f;
    }
  }

  /**
   * Filters the entire source image.
   *
   * This is a convenience function which calls
   * {@link zoom(WritableRaster, Rectangle, BufferedImage, Filter)},
   * setting the destination rectangle as the bounds of the destination
   * tile.
   *
   * @param dst the destination rectangle
   * @param src the soure image
   * @param filter the filter to apply
   * @throws ClassCastException if <code>src</code> does not store its data
   * in a {@link DataBufferInt}
   */
  public static BufferedImage zoom(
    Rectangle dst, BufferedImage src, final Filter filter) {

    final WritableRaster dstR =
      src.getColorModel().createCompatibleWritableRaster(dst.width, dst.height);
    zoom(dstR, dstR.getBounds(), src, filter);

    // FIXME: check whether this affects hardware acceleration
    return new BufferedImage(
      src.getColorModel(),
      dstR,
      src.isAlphaPremultiplied(),
      null
    );
  }

  /**
   * Filters a portion of the source image.
   *
   * @param dstR the destination tile to calculate
   * @param dst_fr the bounds of the whole destination image
   * @param srcI the source image
   * @param filter the filter to apply
   * @throws ClassCastException if <code>srcI</code> does not store its data
   * in a {@link DataBufferInt}
   */
  public static void zoom(
    WritableRaster dstR,
    Rectangle dst_fr,
    BufferedImage srcI,
    final Filter filter)
  {
    final int[] dst_data = ((DataBufferInt) dstR.getDataBuffer()).getData();

    final int src_type;
    if (srcI.getTransparency() == BufferedImage.OPAQUE) {
      src_type = OPAQUE;
    }
    else if (srcI.isAlphaPremultiplied()) {
      src_type = TRANS_PREMULT;
    }
    else {
      src_type = TRANS_UNPREMULT;
    }

    final int dx0 = dstR.getMinX();
    final int dy0 = dstR.getMinY();
    final int dx1 = dx0 + dstR.getWidth() - 1;
    final int dy1 = dy0 + dstR.getHeight() - 1;
    final int dw = dstR.getWidth();
    final int dh = dstR.getHeight();

    final int dstWidth = dst_fr.width;
    final int dstHeight = dst_fr.height;

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

    final int[] src_data =
      ((DataBufferInt) srcI.getRaster().getDataBuffer()).getData();

    resample(
      src_data, false,
      sx0, sy0, sx1, sy1, sw, sh, src_type, srcWidth, srcHeight,
      dst_data, dx0, dy0, dx1, dy1, dw, dh, dstWidth, dstHeight,
      xscale, yscale, filter
    );
  }

  public static final int OPAQUE = 0;
  public static final int TRANS_PREMULT = 1;
  public static final int TRANS_UNPREMULT = 2;

  public static void resample(
    int[] src_data,
    boolean src_data_consecutive,
    int sx0,
    int sy0,
    int sx1,
    int sy1,
    int sw,
    int sh,
    int src_type,
    int srcWidth,   // width of full soruce
    int srcHeight,  // height of full source
    int[] dst_data,
    int dx0,
    int dy0,
    int dx1,
    int dy1,
    int dw,
    int dh,
    int dstWidth,   // width of full destination
    int dstHeight,  // height of full destination
    float xscale,
    float yscale,
    final Filter filter)
  {
    final int[] work = new int[sh];

    final float fwidth = filter.getSamplingRadius();

    final CList[] ycontrib =
      calc_contrib(dh, fwidth, yscale, dy0, sy0, sh, filter);
    final CList[] xcontrib =
      calc_contrib(dw, fwidth, xscale, dx0, sx0, sw, filter);

    // apply the filter
    switch (src_type) {
    case OPAQUE:
      // handle TYPE_INT_RGB, TYPE_INT_BGR
      if (src_data_consecutive) {
        for (int dx = 0; dx < dw; ++dx) {
          apply_h_opaque(0, 0, sh, sw, xcontrib[dx], src_data, work);
          apply_v_opaque(dh, ycontrib, work, dst_data, dx, dw);
        }
      }
      else {
        for (int dx = 0; dx < dw; ++dx) {
          apply_h_opaque(sx0, sy0, sh, srcWidth, xcontrib[dx], src_data, work);
          apply_v_opaque(dh, ycontrib, work, dst_data, dx, dw);
        }
      }
      break;
    case TRANS_PREMULT:
      // handle TYPE_INT_ARGB_PRE
      if (src_data_consecutive) {
        for (int dx = 0; dx < dw; ++dx) {
          apply_h(0, 0, sh, sw, xcontrib[dx], src_data, work);
          apply_v(dh, ycontrib, work, dst_data, dx, dw);
        }
      }
      else {
        for (int dx = 0; dx < dw; ++dx) {
          apply_h(sx0, sy0, sh, srcWidth, xcontrib[dx], src_data, work);
          apply_v(dh, ycontrib, work, dst_data, dx, dw);
        }
      }
      break;
    case TRANS_UNPREMULT:
      // handle TYPE_INT_ARGB

      // premultiply (copy of) source data
      final int[] pre_src_data = new int[src_data.length];
      for (int i = 0; i < src_data.length; ++i) {
        final int unpre = src_data[i];
        final int a = (unpre >>> 24) & 0xff;

        if (a == 255) {
          pre_src_data[i] = unpre;
        }
        else {
          final float na = a / 255.0f;

          pre_src_data[i] =
            a << 24 |
            ((int)(((unpre >>> 16) & 0xff) * na + 0.5f)) << 16 |
            ((int)(((unpre >>>  8) & 0xff) * na + 0.5f)) <<  8 |
            ((int)(((unpre       ) & 0xff) * na + 0.5f));
        }
      }

      if (src_data_consecutive) {
        for (int dx = 0; dx < dw; ++dx) {
          apply_h(0, 0, sh, sw, xcontrib[dx], pre_src_data, work);
          apply_v(dh, ycontrib, work, dst_data, dx, dw);
        }
      }
      else {
        for (int dx = 0; dx < dw; ++dx) {
          apply_h(sx0, sy0, sh, srcWidth, xcontrib[dx], pre_src_data, work);
          apply_v(dh, ycontrib, work, dst_data, dx, dw);
        }
      }

      // unpremultiply destination data
      for (int i = 0; i < dst_data.length; ++i) {
        final int pre = dst_data[i];
        final int a = (pre >>> 24) & 0xff;

        if (a == 255) {
          continue;
        }
        else {
          final float inv_na = 255.0f / a;

          dst_data[i] =
            a << 24 |
            ((int)(((pre >>> 16) & 0xff) * inv_na + 0.5f)) << 16 |
            ((int)(((pre >>>  8) & 0xff) * inv_na + 0.5f)) <<  8 |
            ((int)(((pre       ) & 0xff) * inv_na + 0.5f));
        }
      }
      break;
    default:
      throw new IllegalArgumentException();
    }
  }

  private static CList[] calc_contrib(
    final int dl,         // dst length along this axis
    final float fwidth,   // filter width along this axis
    final float scale,    // scale factor along this axis
    final int d0,         // dst initial
    final int s0,         // src initial
    final int sl,         // src length along this axis
    final Filter filter)
  {
    // Calculate filter contributions for each destination strip
    final CList[] contrib = new CList[dl];
    for (int i = 0; i < contrib.length; i++) contrib[i] = new CList();

    final float blur = 1.0f;
    final float kscale = 1.0f/(blur*Math.max(1.0f/scale, 1.0f));
    final float width = fwidth / kscale;

    for (int i = 0; i < dl; i++) {
      final float center = (i+d0+0.5f) / scale;
      final int start = (int) Math.max(center-width+0.5f, s0);
      final int stop = (int) Math.min(center+width+0.5f, s0+sl);
      final int numContrib = stop - start;

      contrib[i].n = numContrib;
      contrib[i].pixel = start - s0;
      contrib[i].weight = new float[numContrib];

      float density = 0.0f;
      for (int n = 0; n < numContrib; n++) {
        density += contrib[i].weight[n] =
          filter.apply(kscale*(start+n-center+0.5f));
      }

      if (density != 0.0f && density != 1.0f) {
        for (int j = 0; j < numContrib; j++) {
          contrib[i].weight[j] /= density;
        }
      }
    }

    return contrib;
  }

  private static void apply_h(
    final int sx0,
    final int sy0,
    final int sh,
    final int stride,
    final CList xcontrib,
    final int[] src,
    final int[] work)
  {
    final CList c = xcontrib;
    final int max = c.n;

    final int base = sx0 + c.pixel + sy0*stride;

    // Apply pre-computed filter to sample horizontally from src to work
    for (int k = 0; k < sh; k++) {
      float s_a = 0.0f;  // alpha sample
      float s_r = 0.0f;  // red sample
      float s_g = 0.0f;  // green sample
      float s_b = 0.0f;  // blue sample

      final int pos = base + k*stride;

      final int pel = src[pos];
      boolean bPelDelta = false;

      // Check for areas of constant color. It is *much* faster to
      // to check first and then calculate weights only if needed.
      for (int j = 0; j < max; j++) {
        if (c.weight[j] == 0.0f) continue;
        if (src[pos + j] != pel) {
          bPelDelta = true;
          break;
        }
      }

      if (bPelDelta) {
        // There is a color change from 0 to max; we need to use weights.
        for (int j = 0; j < max; j++) {
          final float w = c.weight[j];
          final int sd = src[pos + j];

          s_a += ((sd >>> 24) & 0xff) * w;
          s_r += ((sd >>> 16) & 0xff) * w;
          s_g += ((sd >>>  8) & 0xff) * w;
          s_b += ((sd       ) & 0xff) * w;
        }

        // Ugly, but fast.
        work[k] =
          (s_a > 255 ? 255 : s_a < 0 ? 0 : (int)(s_a+0.5f)) << 24 |
          (s_r > 255 ? 255 : s_r < 0 ? 0 : (int)(s_r+0.5f)) << 16 |
          (s_g > 255 ? 255 : s_g < 0 ? 0 : (int)(s_g+0.5f)) <<  8 |
          (s_b > 255 ? 255 : s_b < 0 ? 0 : (int)(s_b+0.5f));
      }
      else {
        // If there's no color change from 0 to max, maintain that.
        work[k] = pel;
      }
    }
  }

  private static void apply_h_opaque(
    final int sx0,
    final int sy0,
    final int sh,
    final int stride,
    final CList xcontrib,
    final int[] src,
    final int[] work)
  {
    final CList c = xcontrib;
    final int max = c.n;

    final int base = sx0 + c.pixel + sy0*stride;

    // Apply pre-computed filter to sample horizontally from src to work
    for (int k = 0; k < sh; k++) {
      float s_r = 0.0f;  // red sample
      float s_g = 0.0f;  // green sample
      float s_b = 0.0f;  // blue sample

      final int pos = base + k*stride;

      final int pel = src[pos];
      boolean bPelDelta = false;

      // Check for areas of constant color. It is *much* faster to
      // to check first and then calculate weights only if needed.
      for (int j = 0; j < max; j++) {
        if (c.weight[j] == 0.0f) continue;
        if (src[pos + j] != pel) {
          bPelDelta = true;
          break;
        }
      }

      if (bPelDelta) {
        // There is a color change from 0 to max; we need to use weights.
        for (int j = 0; j < max; j++) {
          final float w = c.weight[j];
          final int sd = src[pos + j];

          s_r += ((sd >>> 16) & 0xff) * w;
          s_g += ((sd >>>  8) & 0xff) * w;
          s_b += ((sd       ) & 0xff) * w;
        }

        // Ugly, but fast.
        work[k] =
          (s_r > 255 ? 255 : s_r < 0 ? 0 : (int)(s_r+0.5f)) << 16 |
          (s_g > 255 ? 255 : s_g < 0 ? 0 : (int)(s_g+0.5f)) <<  8 |
          (s_b > 255 ? 255 : s_b < 0 ? 0 : (int)(s_b+0.5f));
      }
      else {
        // If there's no color change from 0 to max, maintain that.
        work[k] = pel;
      }
    }
  }

  private static void apply_v(
    final int dh,
    final CList[] ycontrib,
    final int[] work,
    final int[] dst,
    final int dx,
    final int dw)
  {
    // Apply pre-computed filter to sample vertically from work to dst
    for (int i = 0; i < dh; i++) {
      float s_a = 0.0f;  // alpha sample
      float s_r = 0.0f;  // red sample
      float s_g = 0.0f;  // green sample
      float s_b = 0.0f;  // blue sample

      final CList c = ycontrib[i];
      final int max = c.n;
      final int pel = work[c.pixel];
      boolean bPelDelta = false;

      // Check for areas of constant color. It is *much* faster to
      // to check first and then calculate weights only if needed.
      for (int j = 0; j < max; j++) {
        if (c.weight[j] == 0.0f) continue;
        if (work[c.pixel + j] != pel) {
          bPelDelta = true;
          break;
        }
      }

      if (bPelDelta) {
        // There is a color change from 0 to max; we need to use weights.
        for (int j = 0; j < max; j++) {
          final float w = c.weight[j];
          final int wd = work[c.pixel + j];

          s_a += ((wd >>> 24) & 0xff) * w;
          s_r += ((wd >>> 16) & 0xff) * w;
          s_g += ((wd >>>  8) & 0xff) * w;
          s_b += ((wd       ) & 0xff) * w;
        }

        // working in premultiplied domain, must clamp R,G,B to A
        final int a = s_a > 255 ? 255 : s_a < 0 ? 0 : (int)(s_a+0.5f);

        // Ugly, but fast.
        dst[dx + i*dw] =
          a << 24 |
          (s_r > a ? a : s_r < 0 ? 0 : (int)(s_r+0.5f)) << 16 |
          (s_g > a ? a : s_g < 0 ? 0 : (int)(s_g+0.5f)) <<  8 |
          (s_b > a ? a : s_b < 0 ? 0 : (int)(s_b+0.5f));
      }
      else {
        // If there's no color change from 0 to max, maintain that.

        // working in premultiplied domain, must clamp R,G,B to A
        final int a = (pel >>> 24) & 0xff;
        final int r = (pel >>> 16) & 0xff;
        final int g = (pel >>>  8) & 0xff;
        final int b = (pel       ) & 0xff;

        dst[dx + i*dw] =
          a << 24 |
          (r > a ? a : r) << 16 |
          (g > a ? a : g) <<  8 |
          (b > a ? a : b);
      }
    }
  }

  private static void apply_v_opaque(
    final int dh,
    final CList[] ycontrib,
    final int[] work,
    final int[] dst,
    final int dx,
    final int dw)
  {
    // Apply pre-computed filter to sample vertically from work to dst
    for (int i = 0; i < dh; i++) {
      float s_r = 0.0f;  // red sample
      float s_g = 0.0f;  // green sample
      float s_b = 0.0f;  // blue sample

      final CList c = ycontrib[i];
      final int max = c.n;
      final int pel = work[c.pixel];
      boolean bPelDelta = false;

      // Check for areas of constant color. It is *much* faster to
      // to check first and then calculate weights only if needed.
      for (int j = 0; j < max; j++) {
        if (c.weight[j] == 0.0f) continue;
        if (work[c.pixel + j] != pel) {
          bPelDelta = true;
          break;
        }
      }

      if (bPelDelta) {
        // There is a color change from 0 to max; we need to use weights.
        for (int j = 0; j < max; j++) {
          final float w = c.weight[j];
          final int wd = work[c.pixel + j];

          s_r += ((wd >>> 16) & 0xff) * w;
          s_g += ((wd >>>  8) & 0xff) * w;
          s_b += ((wd       ) & 0xff) * w;
        }

        // Ugly, but fast.
        dst[dx + i*dw] =
          (s_r > 255 ? 255 : s_r < 0 ? 0 : (int)(s_r+0.5f)) << 16 |
          (s_g > 255 ? 255 : s_g < 0 ? 0 : (int)(s_g+0.5f)) <<  8 |
          (s_b > 255 ? 255 : s_b < 0 ? 0 : (int)(s_b+0.5f));
      }
      else {
        // If there's no color change from 0 to max, maintain that.
        dst[dx + i*dw] = pel;
      }
    }
  }
}
