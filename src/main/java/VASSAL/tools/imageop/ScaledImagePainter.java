/*
 * Copyright (c) 2007 by Rodney Kinney
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
package VASSAL.tools.imageop;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;

/**
 * Paints an image at arbitrary scale. Uses the ImageOp interfaces to
 * lazily fetch and cache images.
 *
 * @author rodneykinney
 * @since 3.1.0
 */
public class ScaledImagePainter {
  protected ImageOp srcOp;
  protected ScaleOp scaleOp;

  public void setImageName(String imageName) {
    setSource(imageName == null || imageName.trim().length() == 0
      ? null : Op.load(imageName));
  }

  public Dimension getImageSize() {
    return srcOp == null ? new Dimension() : srcOp.getSize();
  }

  public void draw(Graphics g, int x, int y, double scale, ImageObserver obs) {
    if (srcOp == null || scale <= 0) return;

    final BufferedImage img;
    if (scale == 1.0) {
      img = srcOp.getImage();
    }
    else {
      if (scaleOp == null || scaleOp.getScale() != scale) {
        scaleOp = Op.scale(srcOp, scale);
      }
      img = scaleOp.getImage();
    }

    if (img == null) return;

    g.drawImage(img, x, y, obs);
  }

  public ImageOp getSource() {
    return srcOp;
  }

  public void setSource(ImageOp src) {
    this.srcOp = src;
    scaleOp = null;
  }
}
