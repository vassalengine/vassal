/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman
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

package VASSAL.tools;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JFrame;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageUtils;

// FIXME: wizard doesn't get the right icons
// FIXME: check that parentless dialogs get the right icons

public class ApplicationIcons {
  
  public static final String VASSAL_ICON_LARGE = "VASSAL-256x256.png";

  private ApplicationIcons() {}

  //
  // Set up app icons for non-Mac systems
  //

  private static final List<BufferedImage> icons;

  static {
    if (SystemUtils.IS_OS_MAC_OSX) {
      icons = null;
    }
    else {
      // load the icons
      List<BufferedImage> l = null;
      try {
        if (SystemUtils.IS_OS_WINDOWS) {
          // Windows uses 24x24 instead of 22x22
          final BufferedImage src =
            ImageUtils.getImageResource("/icons/22x22/VASSAL.png");
          final BufferedImage dst =
            ImageUtils.createCompatibleTranslucentImage(24, 24);
          final Graphics2D g = dst.createGraphics();
          g.drawImage(src, 1, 1, null);
          g.dispose();

          l = Arrays.asList(
            ImageUtils.getImageResource("/icons/16x16/VASSAL.png"),
            dst,  // 24x24
            ImageUtils.getImageResource("/icons/32x32/VASSAL.png"),
            ImageUtils.getImageResource("/icons/48x48/VASSAL.png"),
            ImageUtils.getImageResource("/images/"+VASSAL_ICON_LARGE)
          );
        }
        else {
          // load the standard Tango sizes
          l = Arrays.asList(
            ImageUtils.getImageResource("/icons/16x16/VASSAL.png"),
            ImageUtils.getImageResource("/icons/22x22/VASSAL.png"),
            ImageUtils.getImageResource("/icons/32x32/VASSAL.png"),
            ImageUtils.getImageResource("/icons/48x48/VASSAL.png")
          );
        }
      }
      catch (ImageIOException e) {
        ReadErrorDialog.error(e, e.getFile());
      }
      icons = l;
    }
  }

  public static void setFor(JFrame w) {
    if (icons != null) {
      w.setIconImages(icons);
    }
  }

  public static void setFor(JDialog w) {
    if (icons != null) {
      w.setIconImages(icons);
    }
  }
}
