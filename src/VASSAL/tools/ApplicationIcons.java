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
import java.awt.Window;
import java.awt.image.BufferedImage;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JFrame;

import org.apache.commons.lang.SystemUtils;

import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.image.ImageUtils;


// FIXME: wizard doesn't get the right icons
// FIXME: check that parentless dialogs get the right icons

public class ApplicationIcons {

  private ApplicationIcons() {}

  //
  // Set up app icons for non-Mac systems
  //
  // The only decent way to set app icons in a variety of sizes is to
  // use Window.setIconImages(), which unforunately is a method new in
  // Java 1.6. We check here whether the method exists and use it if
  // we can. Otherwise, we use the crappy Window.setIconImage() and set
  // a single 16x16 icon. It should not generally be necessary to use
  // setIconImage(), as almost all non-Mac users should be using a Java
  // 6 JRE at this point.
  //
  // FIXME: Use setIconImages() directly on move to Java 1.6 or better
  //

  private static final Method setIconImages;
  private static final List<BufferedImage> icons;

  static {
    if (SystemUtils.IS_OS_MAC_OSX) {
      setIconImages = null;
      icons = null;
    }
    else {
      // get Window.setIconImages() by reflection
      Method m = null;
      try {
        m = Window.class.getMethod("setIconImages", List.class);
      }
      catch (NoSuchMethodException e) {
        // This means we are using Java 1.5.
      }

      setIconImages = m;

      // load the icons
      List<BufferedImage> l = null;
      try {
        if (setIconImages != null) {
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
              ImageUtils.getImageResource("/images/VASSAL-256x256.png")
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
        else {
          // we are using Java 1.5, so we can load but a single humble icon
          l = Collections.singletonList(
            ImageUtils.getImageResource("/icons/16x16/VASSAL.png")
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
    if (icons == null) return;

    if (setIconImages != null) {
      setIconImages(w);
    }
    else {
      // Load a single image as a fallback and watch in horror as it
      // gets scaled to ridiculous sizes.
      w.setIconImage(icons.get(0));
    }
  }

  public static void setFor(JDialog w) {
    if (icons == null) return;

    if (setIconImages != null) {
      setIconImages(w);
    }
    else {
      // Give up. JDialog has no setIconImage() method.
    }
  }

  private static void setIconImages(Window w) {
    try {
      setIconImages.invoke(w, icons);
    }
    catch (IllegalAccessException e) {
      ErrorDialog.bug(e);
    }
    catch (IllegalArgumentException e) {
      ErrorDialog.bug(e);
    }
    catch (InvocationTargetException e) {
      ErrorDialog.bug(e);
    }
    catch (ExceptionInInitializerError e) {
      ErrorDialog.bug(e);
    }
  }
}
