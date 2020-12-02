/*
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
package VASSAL.tools.image;

import java.awt.Image;
import java.awt.image.AbstractMultiResolutionImage;
import java.awt.image.BufferedImage;
import java.util.List;
import java.util.Objects;

public class MultiResolutionRenderedImage extends AbstractMultiResolutionImage {
  @FunctionalInterface
  public interface Renderer {
    BufferedImage render(int w, int h, double scale);
  } 

  private final int baseWidth;
  private final int baseHeight;
  private final Renderer renderer;

  public MultiResolutionRenderedImage(int w, int h, Renderer r) {
    baseWidth = w;
    baseHeight = h;
    renderer = Objects.requireNonNull(r);
  }

  @Override
  public Image getResolutionVariant(double w, double h) {
    return renderer.render(
      (int) Math.round(w),
      (int) Math.round(h),
      w / baseWidth
    );
  }

  @Override
  public List<Image> getResolutionVariants() {
    return List.of(getBaseImage());
  }

  @Override
  protected Image getBaseImage() {
    return renderer.render(baseWidth, baseHeight, 1.0);
  }
}
