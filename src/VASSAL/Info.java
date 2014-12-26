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

import org.apache.commons.lang.SystemUtils;

import VASSAL.tools.version.VassalVersionTokenizer;
import VASSAL.tools.version.VersionFormatException;
import VASSAL.tools.version.VersionTokenizer;

/**
 * Class for storing release-related information
 */
public final class Info {
  private static final String VERSION = "3.2.15"; //$NON-NLS-1$

  // Do not allow editing of modules with this revision or later
  private static final String EXPIRY_VERSION = "3.3";  //$NON-NLS-1$

  // Warn about editing modules, saves, logs written before this version
  private static final String UPDATE_VERSION =  "3.2";

  private static File homeDir;
  private static File tmpDir;

  /** The path to the JVM binary. */
  public static final String javaBinPath =
    new StringBuilder(System.getProperty("java.home"))
        .append(File.separator)
        .append("bin")
        .append(File.separator)
        .append("java")
        .toString();

  /** This class should not be instantiated */
  private Info() { }

  /**
   * A valid version format is "w.x.[y|bz]", where 'w','x','y', and 'z' are
   @* integers. In the version number, w.x are the major/minor release number,
   * y is the bug-fix release number, and the 'b' indicates a beta release,
   * e.g. 3.0b2.
   *
   * @return the version of the VASSAL engine.
   */
  public static String getVersion() {
    return VERSION;
  }

  /**
   * The major/minor portion of the release version. If the version is a
   * beta-release number, a 'beta' is appended. For example, the minor
   * version of 3.0.2 is 3.0, and the minor version of 3.0b3 is 3.0beta.
   *
   * @return
   * @deprecated If you need the minor version number, get it from
   * a {@link VersionTokenizer}.
   */
  @Deprecated
  public static String getMinorVersion() {
    final VersionTokenizer tok = new VassalVersionTokenizer(VERSION);
    try {
      return Integer.toString(tok.next()) + "." +
             Integer.toString(tok.next());
    }
    catch (VersionFormatException e) {
      return null;
    }
  }

  private static final int instanceID;

  // Set the instance id from the system properties.
  static {
    final String idstr = System.getProperty("VASSAL.id");
    if (idstr == null) {
      instanceID = 0;
    }
    else {
      int id;
      try {
        id = Integer.parseInt(idstr);
      }
      catch (NumberFormatException e) {
        id = -1;
      }

      instanceID = id;
    }
  }

  /**
   * Returns the instance id for this process. The instance id will be
   * be unique across the Module Manager and its children.
   */
  public static int getInstanceID() {
    return instanceID;
  }

  /**
   * Get size of screen accounting for the screen insets (i.e. Windows taskbar)
   *
   * @return
   */
  public static Rectangle getScreenBounds(Component c) {
    final Rectangle bounds =
      new Rectangle(Toolkit.getDefaultToolkit().getScreenSize());
    final GraphicsConfiguration config = c.getGraphicsConfiguration();
    final Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(config);
    bounds.translate(insets.left, insets.top);
    bounds.setSize(bounds.width - insets.left - insets.right,
                   bounds.height - insets.top - insets.bottom);
    return bounds;
  }

  /** @depricated Use {@link SystemUtils.IS_OS_MAC_OSX} instead */
  @Deprecated
  public static boolean isMacOSX() {
    return SystemUtils.IS_OS_MAC_OSX;
  }

  /** @depricated Use {@link SystemUtils.IS_OS_MAC_OSX} instead. */
  @Deprecated
  public static boolean isMacOsX() {
    return SystemUtils.IS_OS_MAC_OSX;
  }

  /** @depricated Use {@link SystemUtils.IS_OS_WINDOWS} instead */
  @Deprecated
  public static boolean isWindows() {
    return SystemUtils.IS_OS_WINDOWS;
  }

  public static boolean isModuleTooNew(String version) {
    return compareVersions(version, EXPIRY_VERSION) >= 0;
  }

  public static boolean hasOldFormat(String version) {
    return compareVersions(version, UPDATE_VERSION) < 0;
  }

  /**
   * Compares VASSAL version strings. This method is guaranteed to
   * correctly compare the current version string with any other
   * version string. It is <em>not</em> guaranteed to correctly
   * compare two arbitrary version strings.
   *
   * @return negative if {@code v0 < v1}, positive if {@code v0 > v1}, and
   * zero if {@code v0 == v1} or if the ordering cannot be determined from
   * the parseable parts of the two <code>String</code>s.
   */
  public static int compareVersions(String v0, String v1) {
    final VersionTokenizer tok0 = new VassalVersionTokenizer(v0);
    final VersionTokenizer tok1 = new VassalVersionTokenizer(v1);

    try {
      // find the first token where v0 and v1 differ
      while (tok0.hasNext() && tok1.hasNext()) {
        final int n0 = tok0.next();
        final int n1 = tok1.next();

        if (n0 != n1) return n0 - n1;
      }
    }
    catch (VersionFormatException e) {
      return 0;
    }

    // otherwise, the shorter one is earlier; or they're the same
    return tok0.hasNext() ? 1 : (tok1.hasNext() ? -1 : 0);
  }

  /**
   * Returns the directory where VASSAL is installed.
   *
   * @return a {@link File} representing the directory
   */
  public static File getBaseDir() {
    return new File(System.getProperty("user.dir"));
  }

  /**
   * Returns the directory where the VASSAL documentation is installed.
   *
   * @return a {@link File} representing the directory
   * @deprecated Use {@link getDocDir()} instead.
   */
  @Deprecated
  public static File getDocsDir() {
    return getDocDir();
  }

// FIXME: we should have something like
// getBinDir(), getDocDir(), getConfDir(), getTmpDir()

  public static File getBinDir() {
    return new File(System.getProperty("user.dir"));
  }

  public static File getDocDir() {
    final String d = SystemUtils.IS_OS_MAC_OSX ?
      "Contents/Resources/doc" : "doc";
    return new File(getBaseDir(), d);
  }

  public static File getConfDir() {
    return getHomeDir();
  }

  public static File getTempDir() {
    if (tmpDir == null) {
      tmpDir = new File(getHomeDir(), "tmp");

      if (!tmpDir.exists()) {
        tmpDir.mkdirs();
      }
    }

    return tmpDir;
  }

  public static File getPrefsDir() {
    return new File(getConfDir(), "prefs");
  }

// FIXME: this is a misleading name for this function
  public static File getHomeDir() {
// FIXME: creation of VASSAL's home should be moved someplace else, possibly
// to the new Application class when it's merged with the trunk.
    if (homeDir == null) {
      if (SystemUtils.IS_OS_MAC_OSX) {
        homeDir = new File(
          System.getProperty("user.home"), "Library/Application Support/VASSAL"
        );
      }
      else if (SystemUtils.IS_OS_WINDOWS) {
        homeDir = new File(System.getenv("APPDATA") + "/VASSAL");
      }
      else {
        homeDir = new File(System.getProperty("user.home"), ".VASSAL");
      }

      if (!homeDir.exists()) {
// FIXME: What if this fails? This should be done from someplace that
// can signal failure properly.
        homeDir.mkdirs();
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
    return true;
  }

  /**
   * @deprecated since Java 1.4 is now required
   * @return true if this platform supports Java2D
   */
  @Deprecated
  public static boolean is2dEnabled() {
    return true;
  }
}
