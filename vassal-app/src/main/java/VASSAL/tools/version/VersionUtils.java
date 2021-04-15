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
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import org.apache.commons.io.IOUtils;

import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.DefaultArtifactVersion;
import org.apache.maven.artifact.versioning.ComparableVersion;

import VASSAL.Info;

public class VersionUtils {
  protected VersionUtils() {}

  private static final String baseURL = "http://www.vassalengine.org/util/"; //NON-NLS

  private static final String currentRelease = "current-release"; //NON-NLS
  private static final String currentBeta = "current-beta"; //NON-NLS

  private static String release = null;
  private static String beta = null;

  private static String getRelease() throws IOException {
    if (release == null) release = getVersion(baseURL + currentRelease);
    return release;
  }

  private static String getBeta() throws IOException {
    if (beta == null) beta = getVersion(baseURL + currentBeta);
    return beta;
  }

  private static String getVersion(String url) throws IOException {
    try (InputStream in = new URL(url).openStream()) {
      return IOUtils.toString(in, StandardCharsets.UTF_8).trim();
    }
  }

  public static int compareReportable(String version) throws IOException {
    // a version is reportable if it is the current release or beta
    return compareCurrent(version);
  }

  public static boolean isCurrent(String version) throws IOException {
    return compareCurrent(version) >= 0;
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

  private static int compareCurrent(String version) throws IOException {
    switch (signum(compareVersions(version, getRelease()))) {
    case -1: // version is older than the current release
      return -1;
    case  0: // version is the current release
      return 0;
    case  1:
      // version is newer than the current release
      return signum(compareVersions(version, getBeta()));
    }

    throw new IllegalStateException();  // impossible
  }

  public static Boolean isUpdateable(String runningVersion) throws IOException {
    return compareCurrent(runningVersion) < 0;
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

  public static void main(String[] args) throws IOException {
    System.out.println(Info.getVersion() + " is current? " + isCurrent(Info.getVersion())); //NON-NLS
  }
}
