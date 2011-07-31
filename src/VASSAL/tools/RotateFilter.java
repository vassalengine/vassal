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
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.image.ColorModel;
import java.awt.image.FilteredImageSource;
import java.awt.image.ImageConsumer;
import java.awt.image.ImageFilter;
import java.awt.image.ImageProducer;

/** @deprecated Use {@link VASSAL.tools.imageop.RotateOp} instead. */
@Deprecated
public class RotateFilter extends ImageFilter {
  private double angle;
  private double cos, sin;
  private Rectangle rotatedSpace;
  private Rectangle originalSpace;
  private ColorModel defaultRGBModel;
  private int inPixels[], outPixels[];

  public RotateFilter(double angle) {
    this.angle = angle * (Math.PI / 180);
    cos = Math.cos(this.angle);
    sin = Math.sin(this.angle);
    defaultRGBModel = ColorModel.getRGBdefault();
  }

  private void transform(int x, int y, double out[]) {
    out[0] = (x * cos) + (y * sin);
    out[1] = (y * cos) - (x * sin);
  }

  private void transformBack(int x, int y, double out[]) {
    out[0] = (x * cos) - (y * sin);
    out[1] = (y * cos) + (x * sin);
  }

  public void transformSpace(Rectangle rect) {
    double out[] = new double[2];
    double minx = Double.MAX_VALUE;
    double miny = Double.MAX_VALUE;
    double maxx = Double.MIN_VALUE;
    double maxy = Double.MIN_VALUE;
    int w = rect.width;
    int h = rect.height;
    int x = rect.x;
    int y = rect.y;
    for (int i = 0; i < 4; i++) {
      switch (i) {
        case 0:
          transform(x + 0, y + 0, out);
          break;
        case 1:
          transform(x + w-1, y + 0, out);
          break;
        case 2:
          transform(x + 0, y + h-1, out);
          break;
        case 3:
          transform(x + w-1, y + h-1, out);
          break;
      }
      minx = Math.min(minx, out[0]);
      miny = Math.min(miny, out[1]);
      maxx = Math.max(maxx, out[0]);
      maxy = Math.max(maxy, out[1]);
    }
/*
    rect.x = (int) Math.floor(minx);
    rect.y = (int) Math.floor(miny);
    rect.width = (int) Math.ceil(maxx) - rect.x;
    rect.height = (int) Math.ceil(maxy) - rect.y;
*/
    rect.x = (int) Math.round(minx);
    rect.y = (int) Math.round(miny);
    rect.width = (int) Math.round(maxx) - rect.x+1;
    rect.height = (int) Math.round(maxy) - rect.y+1;
  }

  /**
   * Tell the consumer the new dimensions based on our
   * rotation of coordinate space.
   * @see ImageConsumer#setDimensions
   */
  public void setDimensions(int width, int height) {
    originalSpace = new Rectangle(0, 0, width, height);
    rotatedSpace = new Rectangle(0, 0, width, height);
    transformSpace(rotatedSpace);
    inPixels = new int[originalSpace.width * originalSpace.height];
    consumer.setDimensions(rotatedSpace.width, rotatedSpace.height);
  }

  /**
   * Tell the consumer that we use the defaultRGBModel color model
   * NOTE: This overrides whatever color model is used underneath us.
   * @param model contains the color model of the image or filter
   * beneath us (preceding us)
   * @see ImageConsumer#setColorModel
   */
  public void setColorModel(ColorModel model) {
    consumer.setColorModel(defaultRGBModel);
  }

  /**
   * Set the pixels in our image array from the passed
   * array of bytes. Xlate the pixels into our default
   * color model (RGB).
   * @see ImageConsumer#setPixels
   */
  public void setPixels(int x, int y, int w, int h,
                        ColorModel model, byte pixels[],
                        int off, int scansize) {
    int index = y * originalSpace.width + x;
    int srcindex = off;
    int srcinc = scansize - w;
    int indexinc = originalSpace.width - w;
    for (int dy = 0; dy < h; dy++) {
      for (int dx = 0; dx < w; dx++) {
        inPixels[index++] = model.getRGB(pixels[srcindex++] & 0xff);
      }
      srcindex += srcinc;
      index += indexinc;
    }
  }

  /**
   * Set the pixels in our image array from the passed
   * array of integers. Xlate the pixels into our default
   * color model (RGB).
   * @see ImageConsumer#setPixels
   */
  public void setPixels(int x, int y, int w, int h,
                        ColorModel model, int pixels[],
                        int off, int scansize) {
    int index = y * originalSpace.width + x;
    int srcindex = off;
    int srcinc = scansize - w;
    int indexinc = originalSpace.width - w;
    for (int dy = 0; dy < h; dy++) {
      for (int dx = 0; dx < w; dx++) {
        inPixels[index++] = model.getRGB(pixels[srcindex++]);
      }
      srcindex += srcinc;
      index += indexinc;
    }
  }

  /**
   * Notification that the image is complete and there will
   * be no further setPixel calls.
   * @see ImageConsumer#imageComplete
   */
  public void imageComplete(int status) {
    if (status == IMAGEERROR || status == IMAGEABORTED) {
      consumer.imageComplete(status);
      return;
    }
    double point[] = new double[2];
    int srcwidth = originalSpace.width;
    int srcheight = originalSpace.height;
    int outwidth = rotatedSpace.width;
    int outheight = rotatedSpace.height;
    int outx, outy, srcx, srcy;
    outPixels = new int[outwidth * outheight];
    outx = rotatedSpace.x;
    outy = rotatedSpace.y;
    int index = 0;
    for (int y = 0; y < outheight; y++) {
      for (int x = 0; x < outwidth; x++) {
        // find the originalSpace point
        transformBack(outx + x, outy + y, point);
        srcx = (int) Math.round(point[0]);
        srcy = (int) Math.round(point[1]);
        // if this point is within the original image
        // retreive its pixel value and store in output
        // else write a zero into the space. (0 alpha = transparent)
        if (srcx < 0 || srcx >= srcwidth ||
          srcy < 0 || srcy >= srcheight) {
          outPixels[index++] = 0;
        }
        else {
          outPixels[index++] = inPixels[(srcy * srcwidth) + srcx];
        }
      }
    }
    // write the entire new image to the consumer
    consumer.setPixels(0, 0, outwidth, outheight, defaultRGBModel,
                       outPixels, 0, outwidth);
    // tell consumer we are done
    consumer.imageComplete(status);
  }

  public static void main(String args[]) {
    final Image unrotated = Toolkit.getDefaultToolkit().getImage("ASL/images/Climb1d.gif");

    ImageFilter filter = new RotateFilter(-60.0);
    ImageProducer producer = new FilteredImageSource(unrotated.getSource(), filter);
    final Image rotated = new javax.swing.JLabel().createImage(producer);
    javax.swing.JFrame f = new javax.swing.JFrame() {
      private static final long serialVersionUID = 1L;

      public void paint(Graphics g) {
        g.setColor(Color.blue);
        g.fillRect(0, 0, getSize().width, getSize().height);
        g.drawImage(rotated, 100, 100, this);
        g.drawImage(unrotated, 0, 0, this);
        g.drawImage(unrotated,
                    100 + unrotated.getWidth(this),
                    unrotated.getHeight(this),
                    100, 0,
                    0, 0,
                    0 + unrotated.getWidth(this),
                    unrotated.getHeight(this),
                    this);
      }
    };
    f.setSize(300, 300);
    f.setVisible(true);
  }
}
