/*
 * Copyright (c) 2008-2021 by Joel Uckelman
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
package VASSAL.tools.version;

import static java.lang.Integer.signum;

import java.io.IOException;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.ComparableVersion;

public class VersionUtils {
  protected VersionUtils() {}

  private static VersionInfo vinfo = new LiveVersionInfo();

  public static void setVersionInfo(VersionInfo vi) {
    vinfo = vi;
  }

  public static int compareReportable(String version) throws IOException {
    // a version is reportable if it is the current release or beta
    switch (signum(compareVersions(version, vinfo.getRelease()))) {
    case -1: // version is older than the current release
      return -1;
    case 0: // version is the current release
      return 0;
    case 1:
      // version is newer than the current release
      return signum(compareVersions(version, vinfo.getBeta()));
    }

    throw new IllegalStateException();  // impossible
  }

  public static boolean isCurrent(String version) throws IOException {
    return compareReportable(version) >= 0;
  }

  /** @deprecated Use !{@link isCurrent(String)} instead */
  @Deprecated(since = "2021-04-15", forRemoval = true)
  public static Boolean isUpdateable(String runningVersion) throws IOException {
    return !isCurrent(runningVersion);
  }

  /**
   * Compares version strings.
   *
   * @return negative if {@code v0 < v1}, positive if {@code v0 > v1}, and
   * zero if {@code v0 == v1} or if the ordering cannot be determined from
   * the parsable parts of the two <code>String</code>s.
   */
  public static int compareVersions(String v0, String v1) {
    final ComparableVersion comparableVersion0 = new ComparableVersion(v0);
    final ComparableVersion comparableVersion1 = new ComparableVersion(v1);
    return comparableVersion0.compareTo(comparableVersion1);
  }

  public static String nextMinorVersion(String v) {
    final ArtifactVersion av = new DefaultArtifactVersion(v);
    return String.valueOf(av.getMajorVersion()) + '.' +
      (av.getMinorVersion() + 1);
  }

  public static String truncateToMinorVersion(String v) {
    final ArtifactVersion av = new DefaultArtifactVersion(v);
    return String.valueOf(av.getMajorVersion()) + '.' +
      av.getMinorVersion();
  }

  public static String truncateToIncrementalVersion(String v) {
    final ArtifactVersion av = new DefaultArtifactVersion(v);
    return String.valueOf(av.getMajorVersion()) + '.' +
      av.getMinorVersion() + '.' + av.getIncrementalVersion();
  }
}
