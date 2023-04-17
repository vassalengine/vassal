/*
 *
 * Copyright (c) 2020 by Joel Uckelman
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

import java.awt.Image;
import java.awt.image.AbstractMultiResolutionImage;
import java.util.List;

import VASSAL.tools.image.ImageUtils;

public class OpMultiResolutionImage extends AbstractMultiResolutionImage {
  protected ImageOp sop;

  public OpMultiResolutionImage(ImageOp sop) {
    this.sop = sop;
  }

  @Override
  public Image getResolutionVariant(double w, double h) {
    return Op.scale(sop, w / sop.getWidth()).getImage();
  }

  @Override
  public List<Image> getResolutionVariants() {
    return List.of(getBaseImage());
  }

  @Override
  protected Image getBaseImage() {
    final Image base = sop.getImage();
    return base == null ? ImageUtils.NULL_IMAGE : sop.getImage();
  }
}
