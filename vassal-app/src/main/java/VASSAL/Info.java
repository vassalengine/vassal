/*
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
import java.awt.Rectangle;
import java.io.File;

import org.apache.commons.lang3.SystemUtils;

import VASSAL.launch.Config;
import VASSAL.launch.DummyConfig;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.version.VassalVersionTokenizer;
import VASSAL.tools.version.VersionFormatException;
import VASSAL.tools.version.VersionTokenizer;
import VASSAL.tools.version.VersionUtils;
import VASSAL.tools.swing.SwingUtils;

/**
 * Class for storing release-related information
 */
public final class Info {
  private static Config CONFIG = new DummyConfig();

  public static void setConfig(Config c) {
    CONFIG = c;
  }

  /**
   * The path to the JVM binary.
   *
   * @deprecated Use {@link #getJavaBinPath()} instead.
   */
  @Deprecated(since = "2020-10-03", forRemoval = true)
  public static final String javaBinPath = getJavaBinPath().getAbsolutePath();

  /** This class should not be instantiated */
  private Info() { }

  /**
   * A valid version format is "w.x.y[-z]", where 'w','x', and 'y' are
   * integers and z is a string. In the version number, w.x are the
   * major/minor release number, y is the bug-fix release number, and the 'z'
   * identifies an intermediate build: e.g., 3.3.3-alpha1 or 3.3.3-SNAPSHOT
   *
   * @return the full version of the VASSAL engine.
   */
  public static String getVersion() {
    return CONFIG.getVersion();
  }

  /**
   * @return The major/minor portion of the release version. If the version is a
   * beta-release number, a 'beta' is appended. For example, the minor
   * version of 3.0.2 is 3.0, and the minor version of 3.0b3 is 3.0beta.
   *
   * @deprecated If you need the minor version number, get it from
   * a {@link VersionTokenizer}.
   */
  @SuppressWarnings("removal")
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static String getMinorVersion() {
    ProblemDialog.showDeprecated("2020-08-06");
    final VersionTokenizer tok = new VassalVersionTokenizer(getVersion());
    try {
      return tok.next() + "." + tok.next();
    }
    catch (VersionFormatException e) {
      return null;
    }
  }

  /**
   * Bugzilla (and other potential external reporting tools) require only the
   * primary numeric portion of the version number: e.g., 3.3.3-SNAPSHOT
   * return 3.3.3.
   *
   * @return The reportable version number
   */
  public static String getReportableVersion() {
    return CONFIG.getReportableVersion();
  }

  /**
   * @return a version-specific errorLog path
   */
  public static File getErrorLogPath() {
    return CONFIG.getErrorLogPath().toFile();
  }

  public static File getJavaBinPath() {
    return CONFIG.getJavaBinPath().toFile();
  }

  /**
   * Returns the directory where VASSAL is installed.
   *
   * @return a {@link File} representing the directory
   */
  public static File getBaseDir() {
    return CONFIG.getBaseDir().toFile();
  }

  public static File getDocDir() {
    return CONFIG.getDocDir().toFile();
  }

  public static File getConfDir() {
    return CONFIG.getConfDir().toFile();
  }

  public static File getTempDir() {
    return CONFIG.getTempDir().toFile();
  }

  public static File getPrefsDir() {
    return CONFIG.getPrefsDir().toFile();
  }

  /**
   * @return size of screen accounting for the screen insets (e.g., Windows
   * taskbar)
   * @Deprecated Use {@link VASSAL.tools.swing.SwingUtils.getScreenBounds}
   * instead.
   */
  @Deprecated(since = "2020-10-03", forRemoval = true)
  public static Rectangle getScreenBounds(Component c) {
    ProblemDialog.showDeprecated("2020-10-03");
    return SwingUtils.getScreenBounds(c);
  }

  /** @deprecated Use {@link SystemUtils#IS_OS_MAC} instead */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static boolean isMacOSX() {
    ProblemDialog.showDeprecated("2020-08-06");
    return SystemUtils.IS_OS_MAC;
  }

  /** @deprecated Use {@link SystemUtils#IS_OS_MAC} instead. */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static boolean isMacOsX() {
    ProblemDialog.showDeprecated("2020-08-06");
    return SystemUtils.IS_OS_MAC;
  }

  /** @deprecated Use {@link SystemUtils#IS_OS_WINDOWS} instead */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static boolean isWindows() {
    ProblemDialog.showDeprecated("2020-08-06");
    return SystemUtils.IS_OS_WINDOWS;
  }

  public static boolean isModuleTooNew(String version) {
    return VersionUtils.compareVersions(
      VersionUtils.truncateToMinorVersion(version),
      VersionUtils.nextMinorVersion(getVersion())
    ) >= 0;
  }

  public static boolean hasOldFormat(String version) {
    return VersionUtils.compareVersions(
      VersionUtils.truncateToMinorVersion(version),
      VersionUtils.truncateToMinorVersion(getVersion())
    ) < 0;
  }

  /**
   * Compares VASSAL version strings. This method is guaranteed to
   * correctly compare the current version string with any other
   * version string. It is <em>not</em> guaranteed to correctly
   * compare two arbitrary version strings.
   *
   * @return negative if {@code v0 < v1}, positive if {@code v0 > v1}, and
   * zero if {@code v0 == v1} or if the ordering cannot be determined from
   * the parsable parts of the two <code>String</code>s.
   *
   * @deprecated use {@link VersionUtils#compareVersions(String, String)}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static int compareVersions(String v0, String v1) {
    ProblemDialog.showDeprecated("2020-08-06");
    return VersionUtils.compareVersions(v0, v1);
  }

  /**
   * Returns the directory where the VASSAL documentation is installed.
   *
   * @return a {@link File} representing the directory
   * @deprecated Use {@link #getDocDir()} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static File getDocsDir() {
    ProblemDialog.showDeprecated("2020-08-06");
    return getDocDir();
  }

  /** @deprecated Use {@link #getBaseDir()} instead. */
  @Deprecated(since = "2020-10-03", forRemoval = true)
  public static File getBinDir() {
    ProblemDialog.showDeprecated("2020-10-03");
    return getBaseDir();
  }

  /** @deprecated Use {@link #getConfDir()} instead. */
  @Deprecated(since = "2020-10-02", forRemoval = true)
  public static File getHomeDir() {
    ProblemDialog.showDeprecated("2020-10-02");
    return getConfDir();
  }

  /**
   * @return true if this platform supports Swing Drag and Drop
   * @deprecated Check is no longer necessary since Java 1.4+ is required.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static boolean isDndEnabled() {
    ProblemDialog.showDeprecated("2020-08-06");
    return true;
  }

  /**
   * @deprecated since Java 1.4 is now required
   * @return true if this platform supports Java2D
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static boolean is2dEnabled() {
    ProblemDialog.showDeprecated("2020-08-06");
    return true;
  }
}
