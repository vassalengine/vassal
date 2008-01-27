/*
 * $Id$
 *
 * Copyright (c) 2007 by Joel Uckelman
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
import java.awt.RenderingHints;

/**
 * An {@link ImageOp} which rotates its source.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class RotateOp extends RotateScaleOp {

  /**
   * Constructs an <code>ImageOp</code> which will rotate
   * the image produced by its source <code>ImageOp</code>.
   *
   * @param sop the source operation
   * @param angle the angle of rotation, in degrees
   */
  public RotateOp(ImageOp sop, double angle) {
    super(sop, angle, 0.0);
  }
 
  /**
   * Constructs an <code>ImageOp</code> which will rotate
   * the image produced by its source <code>ImageOp</code>.
   *
   * @param sop the source operation
   * @param angle the angle of rotation, in degrees
   * @param hints rendering hints
   */
  public RotateOp(ImageOp sop, double angle, RenderingHints hints) {
    super(sop, angle, 0.0, hints);
  }
}
