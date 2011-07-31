/*
 * $Id$
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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

import java.awt.RenderingHints;
import java.util.Map;

/**
 * Adds additional hints to {@link RenderingHints}.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 * @deprecated All scaling is now done via the high-quality scaler.
 */
@Deprecated
public class RenderingClues extends RenderingHints {

  private static final int INTKEY_EXT_INTERPOLATION = 0xDEADBEEF;

  /** Extended interpolation hint key. */
  public static final Key KEY_EXT_INTERPOLATION =
    new Key(INTKEY_EXT_INTERPOLATION) {
      public boolean isCompatibleValue(Object val) {
        return val == VALUE_INTERPOLATION_LANCZOS_MITCHELL ||
               val == VALUE_INTERPOLATION_BICUBIC ||
               val == VALUE_INTERPOLATION_BILINEAR ||
               val == VALUE_INTERPOLATION_NEAREST_NEIGHBOR;
      }
    };

  /**
   * Interpolation hint value -- Lanczos interpolation is used for
   * downscaling, while Mitchell interpolation is used for upscaling.
   */
  public static final Object
    VALUE_INTERPOLATION_LANCZOS_MITCHELL = new Object(){};

  /**
   * Constructs an empty collection of <code>RenderingClues</code>.
   */
  public RenderingClues() {
    super(null);
  }

  /**
   *  Constructs a collection of <code>RenderingClues</code> from
   *  the given <code>Map</code>.
   *
   *  @param init a map of key/value pairs to initialize the hints
   */
  public RenderingClues(Map<Key,?> init) {
    super(init);
  }
}
