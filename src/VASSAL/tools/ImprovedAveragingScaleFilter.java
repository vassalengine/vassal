/*
 * $Id$
 *
 * Copyright (c) 2004 by Ben Smith
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

import java.awt.image.AreaAveragingScaleFilter;
import java.awt.image.ColorModel;

/**
 * Extension of java.awt.image.AreaAveragingScaleFilter.  Uses the
 * same algorithm but makes sure all images are scaled using area
 * averaging.  Ensures there is no fallback to ReplicateScaleFilter. */
public class ImprovedAveragingScaleFilter extends AreaAveragingScaleFilter{
  private int savedWidth, savedHeight, savedPixels[];
  private static ColorModel defaultCM = ColorModel.getRGBdefault();

  public ImprovedAveragingScaleFilter(int savedWidth, int savedHeight, int destWidth, int destHeight ) {
    super(destWidth, destHeight);
    this.savedWidth=savedWidth;
    this.savedHeight=savedHeight;
    this.destWidth=destWidth;
    this.destHeight=destHeight;
    savedPixels=new int [savedWidth*savedHeight];
  }

  public void setColorModel (ColorModel model) {
    // Change color model to model you are generating
    consumer.setColorModel (defaultCM);
  }

  public void setHints (int hintflags) {
    consumer.setHints (TOPDOWNLEFTRIGHT | COMPLETESCANLINES |
        SINGLEPASS | (hintflags & SINGLEFRAME));
  }

  public void setPixels (int x, int y, int width, int height,
      ColorModel cm, byte pixels[], int offset, int scansize) {
    setThePixels (x, y, width, height, cm, pixels, offset, scansize);
  }

  public void setPixels (int x, int y, int width, int height,
      ColorModel cm, int pixels[], int offset, int scansize) {
    setThePixels (x, y, width, height, cm, pixels, offset, scansize);
  }

  private void setThePixels (int x, int y, int width, int height,
      ColorModel cm, Object pixels, int offset, int scansize) {

    int sourceOffset = offset;
    int destinationOffset = y * savedWidth + x;
    boolean bytearray = (pixels instanceof byte[]);
    for (int yy=0;yy<height;yy++) {
      for (int xx=0;xx<width;xx++)
        if (bytearray)
          savedPixels[destinationOffset++]=
            cm.getRGB(((byte[])pixels)[sourceOffset++]&0xff);
        else
          savedPixels[destinationOffset++]=
            cm.getRGB(((int[])pixels)[sourceOffset++]);
      sourceOffset += (scansize - width);
      destinationOffset += (savedWidth - width);
    }
  }

  public void imageComplete (int status) {
    if ((status == IMAGEABORTED) || (status == IMAGEERROR)) {
      consumer.imageComplete (status);
      return;
    }
    else {
      // get orig image width and height
      int pixels[] = new int [savedWidth];
      int position;
      for (int yy=0;yy<savedHeight;yy++) {
        position=0;
        int start = yy * savedWidth;
        for (int xx=0;xx<savedWidth;xx++) {
          pixels[position++] = savedPixels[start+xx];
        }
        super.setPixels (0, yy, savedWidth, 1, defaultCM,
            pixels, 0, savedWidth);
      }
      consumer.imageComplete (status);
    }
  }
}
