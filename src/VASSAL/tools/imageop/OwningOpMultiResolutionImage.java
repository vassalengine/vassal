/*
 * $Id$
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

public class OwningOpMultiResolutionImage extends OpMultiResolutionImage {
  protected SortedMap<Double, Image> imgs = new TreeMap<>();

  public OwningOpMultiResolutionImage(ImageOp sop) {
    super(sop);
  }

  @Override
  public Image getResolutionVariant(double w, double h) {
    Image img = imgs.get(w);  
    if (img == null) {
      img = Op.scale(sop, w / sop.getWidth()).getImage();
      imgs.put(w, img);
    }
    return img;
  }

  @Override
  public List<Image> getResolutionVariants() {
    return Collections.unmodifiableList(new ArrayList<>(imgs.values()));
  } 
}
