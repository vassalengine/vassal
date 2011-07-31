/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.PixelGrabber;
import java.awt.image.RGBImageFilter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Sets the transparency of colors in an image
 * @deprecated Use a Graphics2D and an AlphaComposite, or
 * a {@link VASSAL.tools.imageop} instead.
 */
@Deprecated
public class TransparentFilter extends RGBImageFilter {
  private static final Logger logger =
    LoggerFactory.getLogger(TransparentFilter.class);

  private double alpha = 1.0;
  private int[] colors = new int[0];
  private double[] alphas = new double[0];

  public TransparentFilter() {
    this(1.0);
  }

  public TransparentFilter(double alpha) {
    this.alpha = alpha;
  }

  public void setAlpha(double alpha) {
    this.alpha = alpha;
  }

  public void setAlpha(double alpha, int color) {
    for (int i = 0; i < colors.length; ++i) {
      if (color == colors[i]) {
        alphas[i] = alpha;
        return;
      }
    }
    int[] newColors = new int[colors.length + 1];
    System.arraycopy(colors, 0, newColors, 0, colors.length);
    newColors[colors.length] = color;
    colors = newColors;

    double[] newAlphas = new double[alphas.length + 1];
    System.arraycopy(alphas, 0, newAlphas, 0, alphas.length);
    newAlphas[alphas.length] = alpha;
    alphas = newAlphas;
  }

  /**
   * For the given input color, return the color that this color
   * will map to in an offscreen image created by the given Component
   */
  public static int getOffscreenEquivalent(int color, Component obs) {
    Image im = obs.createImage(1, 1);
    Graphics2D g = (Graphics2D) im.getGraphics();
    g.setColor(new java.awt.Color(color));
    g.fillRect(0, 0, 1, 1);
    g.dispose();

    int[] bg = new int[1];
    PixelGrabber pg = new PixelGrabber(im, 0, 0, 1, 1, bg, 0, 1);
    try {
      pg.grabPixels();
    }
    catch (InterruptedException ex) {
      logger.error("", ex);
    }
    return bg[0];
  }

  public void setAlpha(double alpha, Color c) {
    setAlpha(alpha, c.getRGB());
  }

  public int filterRGB(int x, int y, int rgb) {
    double a = alpha;
    for (int i = 0; i < colors.length; ++i) {
      if (rgb == colors[i]) {
        a = alphas[i];
        break;
      }
    }
    rgb = 0xffffff & rgb;
    byte trans = (byte) (a * 0xff);
    return (trans << 24) | rgb;
  }
}
