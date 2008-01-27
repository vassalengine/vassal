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

package VASSAL.tools;

import java.awt.RenderingHints;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.configure.BooleanConfigurer;

/**
 * Adds additional hints to {@link RenderingHints}.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
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

  private static BooleanConfigurer smoothPref;

  /**
   * Returns the default <code>RenderingClues</code>.
   *
   * @return the default <code>RenderingClues</code>
   */
  public static RenderingClues getDefault() {
    RenderingClues rc = new RenderingClues();

// FIXME: does this really belong here?
    if (smoothPref == null) {
      smoothPref = (BooleanConfigurer)
        GameModule.getGameModule()
                  .getPrefs()
                  .getOption(GlobalOptions.SCALER_ALGORITHM);
      if (smoothPref == null) {
        smoothPref = new BooleanConfigurer(null, null, Boolean.FALSE);
      }
      smoothPref.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          // FIXME: a little overbroad?
//          ImageCache.clear();
        }
      });
    }

    rc.put(KEY_EXT_INTERPOLATION, Boolean.TRUE.equals(smoothPref.getValue()) ? 
                                  VALUE_INTERPOLATION_LANCZOS_MITCHELL 
                                : VALUE_INTERPOLATION_BILINEAR);
    rc.put(KEY_INTERPOLATION,     VALUE_INTERPOLATION_BILINEAR);
    rc.put(KEY_ANTIALIASING,      VALUE_ANTIALIAS_ON);
    return rc;
  }
}
