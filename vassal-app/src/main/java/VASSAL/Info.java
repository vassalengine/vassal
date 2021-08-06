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

import VASSAL.launch.Config;
import VASSAL.launch.DummyConfig;
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
   * @deprecated Use {@link VASSAL.tools.swing.SwingUtils.getScreenBounds}
   * instead.
   */
  @Deprecated(since = "2020-10-03", forRemoval = true)
  public static Rectangle getScreenBounds(Component c) {
    return SwingUtils.getScreenBounds(c);
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

  /** @deprecated Use {@link #getBaseDir()} instead. */
  @Deprecated(since = "2020-10-03", forRemoval = true)
  public static File getBinDir() {
    return getBaseDir();
  }

  /** @deprecated Use {@link #getConfDir()} instead. */
  @Deprecated(since = "2020-10-02", forRemoval = true)
  public static File getHomeDir() {
    return getConfDir();
  }
}
