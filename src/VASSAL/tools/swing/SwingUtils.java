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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools.swing;

import java.awt.GraphicsEnvironment;
import java.awt.geom.AffineTransform;

public class SwingUtils {
  // Note: We assume that X and Y scaling will be equal.
  private static final double systemScaling =
    GraphicsEnvironment.isHeadless() ? 1.0 :
      GraphicsEnvironment.getLocalGraphicsEnvironment()
                         .getDefaultScreenDevice()
                         .getDefaultConfiguration()
                         .getDefaultTransform()
                         .getScaleX();

  public static double getSystemScaling() {
    return systemScaling;
  }

  public static AffineTransform descaleTransform(AffineTransform t) {
    return new AffineTransform(
      1.0, 0.0,
      0.0, 1.0,
      t.getTranslateX(), t.getTranslateY()
    );
  }
}
