package VASSAL;

import java.awt.Component;
import java.awt.GraphicsConfiguration;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.util.StringTokenizer;

/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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

/**
 * Class for storing release-related information
 */
public final class Info {
  private static final String VERSION = "2.9b3";
  private static Boolean is2dEnabled;
  private static Boolean isDndEnabled;
  /** This class should not be instantiated */
  private Info() {
  }

  /**
   *
   * @return the version of the VASSAL engine
   */
  public static String getVersion() {
    return VERSION;
  }

  /**
   *
   * @return true if this platform supports Swing Drag and Drop
   */
  public static boolean isDndEnabled() {
    if (isDndEnabled == null) {
      try {
        Class.forName("java.awt.dnd.DropTarget");
        isDndEnabled = Boolean.TRUE;
      }
      catch (ClassNotFoundException e) {
        isDndEnabled = Boolean.FALSE;
      }
    }
    return isDndEnabled.booleanValue();
  }

  /**
   * If running JRE 1.4, we can take into account the screen insets (i.e. Windows taskbar)
   * @return
   */
  public static Rectangle getScreenBounds(Component c) {
    Rectangle bounds =  new Rectangle(Toolkit.getDefaultToolkit().getScreenSize());
    try {
      GraphicsConfiguration config = c.getGraphicsConfiguration();
      Insets insets = (Insets) Toolkit.class.getMethod("getScreenInsets",new Class[]{GraphicsConfiguration.class})
        .invoke(Toolkit.getDefaultToolkit(),new Object[]{config});
      bounds.translate(insets.left,insets.top);
      bounds.setSize(bounds.width-insets.left-insets.right,bounds.height-insets.top-insets.bottom);
    }
    catch (Throwable t) {
    }
    return bounds;
  }

  /**
   * @deprecated since Java 1.4 is now required
   * @return true if this platform supports Java2D
   */
  public static boolean is2dEnabled() {
    if (is2dEnabled == null) {
      try {
        Class.forName("java.awt.Graphics2D");
        is2dEnabled = Boolean.TRUE;
      }
      catch (ClassNotFoundException e) {
        is2dEnabled = Boolean.FALSE;
      }
    }
    return is2dEnabled.booleanValue();
  }

  public static boolean isMacOsX() {
    String os = System.getProperty("os.name");
    return os.toLowerCase().indexOf("mac") >= 0
      && os.toLowerCase().indexOf("x") > 0;
  }

  /**
   *
   * A valid verson format is "w.x.y[bz]", where
   * 'w','x','y', and 'z' are integers.
   * @return a negative number if <code>v2</code> is a later version
   * the <code>v1</code>, a positive number if an earlier version,
   * or zero if the versions are the same.
   *
   */
  public static int compareVersions(String v1, String v2) {
    try {
      int beta1 = v1.indexOf("b");
      int beta2 = v2.indexOf("b");
      if (beta1 > 0) {
        if (beta2 > 0) {
          return compareVersions(v1.substring(0, beta1), v2.substring(0, beta2)) < 0 ?
            -1 : Integer.parseInt(v1.substring(beta1 + 1))
            - Integer.parseInt(v2.substring(beta2 + 1));
        }
        else {
          return compareVersions(v1.substring(0, beta1), v2)
            > 0 ? 1 : -1;
        }
      }
      else if (beta2 > 0) {
        return compareVersions(v1, v2.substring(0, beta2))
          < 0 ? -1 : 1;
      }
      else {
        StringTokenizer s1 = new StringTokenizer(v1, ".");
        StringTokenizer s2 = new StringTokenizer(v2, ".");
        while (s1.hasMoreTokens()
          && s2.hasMoreTokens()) {
          int comp = Integer.parseInt(s1.nextToken())
            - Integer.parseInt(s2.nextToken());
          if (comp != 0) {
            return comp;
          }
        }
        if (s1.hasMoreTokens()) {
          return 1;
        }
        else if (s2.hasMoreTokens()) {
          return -1;
        }
        else {
          return 0;
        }
      }
    }
    catch (NumberFormatException ex) {
      System.err.println("Invalid version format :" + v1 + ", " + v2);
      return 0;
    }
  }

}
