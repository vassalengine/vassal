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
package VASSAL;

import java.awt.Component;
import java.awt.GraphicsConfiguration;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.io.File;

import VASSAL.tools.VersionFormatException;
import VASSAL.tools.VersionTokenizer;

/**
 * Class for storing release-related information
 */
public final class Info {
  private static final String VERSION = "3.1.0-svn2906"; //$NON-NLS-1$
  private static File homeDir;
  private static Boolean is2dEnabled;
  private static Boolean isDndEnabled;

  /** This class should not be instantiated */
  private Info() { }

  /**
   * A valid version format is "w.x.[y|bz]", where 'w','x','y', and 'z' are integers. In the version number, w.x are the
   * major/minor release number, y is the bug-fix release number, and the 'b' indicates a beta release, e.g. 3.0b2
   * 
   * @return the version of the VASSAL engine.
   */
  public static String getVersion() {
    return VERSION;
  }

  /**
   * The major/minor portion of the release version. If the version is a beta-release number, a 'beta' is appended. For
   * example, the minor version of 3.0.2 is 3.0, and the minor version of 3.0b3 is 3.0beta
   * 
   * @return
   */
  public static String getMinorVersion() {
/*
    StringTokenizer st = new StringTokenizer(VERSION, ".b", true); //$NON-NLS-1$
    String minorVersion = st.nextToken() + st.nextToken() + st.nextToken();
    if (st.hasMoreTokens() && "b".equals(st.nextToken())) {
      minorVersion += "beta";
    }
    return minorVersion;
*/
// FIXME: check where this is used. maybe we can deprecate?
    final VersionTokenizer tok = new VersionTokenizer(VERSION);
    try {
      return Integer.toString(tok.next()) +
             Integer.toString(tok.next()) +
             Integer.toString(tok.next());
    }
    catch (VersionFormatException e) {
      return null;
    }
  }

  /**
   * Get size of screen accounting for the screen insets (i.e. Windows taskbar)
   * 
   * @return
   */
  public static Rectangle getScreenBounds(Component c) {
    Rectangle bounds = new Rectangle(Toolkit.getDefaultToolkit().getScreenSize());
    GraphicsConfiguration config = c.getGraphicsConfiguration();
    Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(config);
    bounds.translate(insets.left, insets.top);
    bounds.setSize(bounds.width - insets.left - insets.right,
                   bounds.height - insets.top - insets.bottom);
    return bounds;
  }

  public static boolean isMacOsX() {
    String os = System.getProperty("os.name"); //$NON-NLS-1$
    return os.toLowerCase().indexOf("mac") >= 0 //$NON-NLS-1$
        && os.toLowerCase().indexOf("x") > 0; //$NON-NLS-1$
  }
  
  public static boolean isWindows() {
    return System.getProperty("os.name").toLowerCase().startsWith("windows");
  }

  /**
   * Compares VASSAL version strings.
   *
   * @return negative if {@code v0 > v1}, positive if {@code v0 < v1}, and
   * zero if {@code v0 == v1} or if the ordering cannot be determined from
   * the parseable parts of the two <code>String</code>s.
   */
  public static int compareVersions(String v0, String v1) {
    VersionTokenizer tok0 = new VersionTokenizer(v0);
    VersionTokenizer tok1 = new VersionTokenizer(v1);

    try {
      // find the first token where v0 and v1 differ
      while (tok0.hasNext() && tok1.hasNext()) {
        final int n0 = tok0.next();
        final int n1 = tok1.next();
      
        if (n0 != n1) return n0 - n1;
      }    
    }
    catch (VersionFormatException e) {
      System.err.println("Invalid version format: " +  //$NON-NLS-1$
                         v0 + ", " + v1);              //$NON-NLS-2$
      return 0;
    }

    // otherwise, the shorter one is earlier; or they're the same
    return tok0.hasNext() ? -1 : (tok1.hasNext() ? 1 : 0);
  }

  public static File getHomeDir() {
    if (homeDir == null) {
      homeDir = new File(System.getProperty("user.home"), "VASSAL"); //$NON-NLS-1$ //$NON-NLS-2$
      if (!homeDir.exists()) {
        homeDir.mkdir();
      }
      else if (!homeDir.isDirectory()) {
        homeDir.delete();
        homeDir.mkdir();
      }
    }
    return homeDir;
  }

  /**
   * @return true if this platform supports Swing Drag and Drop
   * @deprecated Check is no longer necessary since Java 1.4+ is required.
   */
  @Deprecated
  public static boolean isDndEnabled() {
    if (isDndEnabled == null) {
      try {
        Class.forName("java.awt.dnd.DropTarget"); //$NON-NLS-1$
        isDndEnabled = Boolean.TRUE;
      }
      catch (ClassNotFoundException e) {
        isDndEnabled = Boolean.FALSE;
      }
    }
    return isDndEnabled.booleanValue();
  }

  /**
   * @deprecated since Java 1.4 is now required
   * @return true if this platform supports Java2D
   */
  @Deprecated
  public static boolean is2dEnabled() {
    if (is2dEnabled == null) {
      try {
        Class.forName("java.awt.Graphics2D"); //$NON-NLS-1$
        is2dEnabled = Boolean.TRUE;
      }
      catch (ClassNotFoundException e) {
        is2dEnabled = Boolean.FALSE;
      }
    }
    return is2dEnabled.booleanValue();
  }
}
