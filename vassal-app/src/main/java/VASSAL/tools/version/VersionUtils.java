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

  public static boolean isCurrent(String version) throws IOException {
    // a version is current if it would update to itself
    return version.equals(update(version));
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

  private static String update(String version) throws IOException {
    String current = VersionUtils.getRelease();
    switch (signum(compareVersions(version, current))) {
    case -1: // version is older than the current release
      return current;
    case  0: // version is the current release
      return version;
    case  1:
      // version is newer than the current release
      current = VersionUtils.getBeta();
      switch (signum(compareVersions(version, current))) {
      case -1:  // version is older than the current beta
        return current;
      case  0:  // version is the current beta
      case  1:  // version is newer than the current beta
        return version;
      }
    }

    throw new IllegalStateException();
  }

  public static Boolean isUpdateable(String runningVersion) throws IOException {
    final String update = update(runningVersion);
    return !update.equals(runningVersion);
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
