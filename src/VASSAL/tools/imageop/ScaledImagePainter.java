/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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
import java.awt.image.ImageObserver;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;

import VASSAL.tools.ErrorLog;

/**
 * Paints an image at arbitrary scale. Uses the ImageOp interfaces to lazily fetch and cache images
 * 
 * @author rodneykinney
 * 
 */
public class ScaledImagePainter {
  protected ImageOp srcOp;
  protected ScaleOp scaleOp;

  public void setImageName(String imageName) {
    ImageOp src = imageName == null || imageName.trim().length() == 0 ? null : Op.load(imageName);
    setSource(src);    
  }

  public Dimension getImageSize() {
    return srcOp == null ? new Dimension() : srcOp.getSize();
  }

  public void draw(Graphics g, int x, int y, double scale, ImageObserver obs) {
    if (srcOp != null) {
      if (scale == 1.0) {
        try {
          g.drawImage(srcOp.getImage(null), x, y, obs);
        }
        catch (CancellationException e) {
          ErrorLog.log(e);
        }
        catch (InterruptedException e) {
          ErrorLog.log(e);
        }
        catch (ExecutionException e) {
          ErrorLog.log(e);
        }
      }
      else {
        if (scaleOp == null || scaleOp.getScale() != scale) {
          scaleOp = Op.scale(srcOp, scale);
        }
        try {
          g.drawImage(scaleOp.getImage(null), x, y, obs);
        }
        catch (CancellationException e) {
          ErrorLog.log(e);
        }
        catch (InterruptedException e) {
          ErrorLog.log(e);
        }
        catch (ExecutionException e) {
          ErrorLog.log(e);
        }
      }
    }
  }

  public ImageOp getSource() {
    return srcOp;
  }
  
  public void setSource(ImageOp src) {
    this.srcOp = src;
    scaleOp = null;
  }
}
