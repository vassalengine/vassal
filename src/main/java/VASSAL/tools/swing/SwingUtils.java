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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools.swing;

import java.awt.Toolkit;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.util.Map;

import org.apache.commons.lang3.SystemUtils;

public class SwingUtils {
  public static AffineTransform descaleTransform(AffineTransform t) {
    return new AffineTransform(
      1.0, 0.0,
      0.0, 1.0,
      t.getTranslateX(), t.getTranslateY()
    );
  }

  public static final Map<?,?> FONT_HINTS = (Map<?,?>) Toolkit.getDefaultToolkit().getDesktopProperty("awt.font.desktophints");

  public static boolean isRightMouseButton(MouseEvent e) {
    if (e.getButton() == MouseEvent.BUTTON3) {
      return true;
    }

    if (!SystemUtils.IS_OS_MAC_OSX) {
      return false;
    }

    return e.getButton() == MouseEvent.BUTTON1 &&
      (e.getModifiersEx() & MouseEvent.CTRL_DOWN_MASK) != 0;
  }
}
